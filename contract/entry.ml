open Environment

(* TODO: optimizations
    - handshake only sometimes
    - rejector also sends mid_state_hash directly
*)

let nil_operations : operation list = []

type requirements =
  | Tez
  | Stake

module Submit (Tezos : Tezos) = struct
  let requires = None

  (* O(log n) *)
  let execute ~submission storage =
    let current_level = Tezos.level in
    (* TODO: this should probably be abstracted *)
    let action_hash = Crypto.blake2b submission in
    let storage = Storage.append_action ~current_level ~action_hash storage in
    (nil_operations, storage)
end

module Join (Tezos : Tezos) = struct
  let requires = Some Tez

  (* O(log n) *)
  let execute storage =
    let sender = Tezos.sender in
    let storage = Storage.join sender storage in
    (nil_operations, storage)
end
module Quit (Tezos : Tezos) = struct
  let requires = Some Stake

  (* O(log n) *)
  let execute storage =
    let sender = Tezos.sender in
    let storage = Storage.burn sender storage in
    let contract =
      match Tezos.get_contract_opt sender with
      | Some contract -> contract
      | None -> failwith "failed to send your money back" in
    let transaction = Tezos.transaction () stake_amount contract in
    ([transaction], storage)
end

module Commit (Tezos : Tezos) = struct
  let requires = Some Stake

  (* O(log n) *)
  let execute ~level ~previous_state_hash ~state_hash ~steps storage =
    let committer = Tezos.sender in
    let current_turn = Turn.current ~current_level:Tezos.level ~level in
    (* TODO: probably not an assert right? *)
    let () = assert (current_turn = Turn.first_committer_turn) in

    let storage =
      match
        Storage.append_commit ~level ~committer ~previous_state_hash ~state_hash
          ~steps storage
      with
      | Some storage -> storage
      | None -> failwith "duplicated committer" in
    (nil_operations, storage)
end
module Reject (Tezos : Tezos) = struct
  let requires = Some Stake

  (* O(log n) *)
  let execute ~level ~committer ~defend_as storage =
    let rejector = Tezos.sender in
    let current_turn = Turn.current ~current_level:Tezos.level ~level in
    (* TODO: this assert is duplicated below *)
    let () = assert (current_turn = Turn.first_rejector_turn) in

    let level_data = Storage.find_level ~level storage in
    let input_hash = Storage.Level_data.input_hash level_data in
    let committer_commit_data =
      match Storage.find_commit ~level ~committer storage with
      | Some commit_data -> commit_data
      | None -> failwith "invalid committer" in
    let rejector_commit_data =
      match Storage.find_commit ~level ~committer:defend_as storage with
      | Some commit_data -> commit_data
      | None -> failwith "invalid defend_as" in

    (* TODO: if the rejector disagrees with previous_state_hash then
           it's just bullshitting, a honest player will never create
           a rejection game against a commit that it doesn't agree *)
    let previous_state_hash =
      Storage.Commit_data.previous_state_hash committer_commit_data in

    let committer_steps = Storage.Commit_data.steps committer_commit_data in
    let rejector_steps = Storage.Commit_data.steps rejector_commit_data in

    let initial_state_hash =
      Vm.make_initial_state_hash ~previous_state_hash ~input_hash in
    let game =
      match
        Temporal_rejection_game.play ~current_turn ~initial_state_hash
          ~committer_steps ~rejector_steps
      with
      | Some game -> game
      | None ->
        (* TODO: this checking is duplicated from above *)
        failwith "you're not allowed to create a game" in

    let storage =
      match Storage.append_game ~level ~committer ~rejector game storage with
      | Some storage -> storage
      | None -> failwith "rejector already has a rejection game" in

    (nil_operations, storage)
end

module Move (Tezos : Tezos) = struct
  let requires = Some Stake

  let on_winner ~level ~committer ~rejector ~winner storage =
    match winner with
    | Committer ->
      (* TODO: removing the game is okay because in case of timeout
               the honest already had a turn to remove it and in case of
               replay there is no point on forking a replay *)
      let storage =
        match Storage.remove_game ~level ~committer ~rejector storage with
        | Some storage -> storage
        | None ->
          (* TODO: this is a leaky abstraction from execute *)
          failwith "unreachable" in
      Storage.burn rejector storage
    | Rejector ->
      let storage =
        match Storage.remove_commit ~level ~committer storage with
        | Some storage -> storage
        | None -> failwith "unreachable" in
      Storage.burn committer storage
  let on_waiting ~level ~committer ~rejector game storage =
    match Storage.update_game ~level ~committer ~rejector game storage with
    | Some storage -> storage
    | None -> failwith "unreachable"

  (* O(log n) *)
  let execute ~level ~committer ~rejector ~player ~move storage =
    let current_turn = Turn.current ~current_level:Tezos.level ~level in
    let game =
      match Storage.find_game ~level ~committer ~rejector storage with
      | Some game -> game
      | None -> failwith "invalid game" in

    let storage =
      match Temporal_rejection_game.move ~current_turn player move game with
      | Move_result_winner winner ->
        on_winner ~level ~committer ~rejector ~winner storage
      | Move_result_waiting game ->
        on_waiting ~level ~committer ~rejector game storage
      | Move_result_not_your_turn -> failwith "not your turn"
      | Move_result_invalid -> failwith "invalid move" in
    (nil_operations, storage)
end
module Defend (Tezos : Tezos) = struct
  module Move = Move (Tezos)
  let requires = Move.requires

  (* O(log n) *)
  let execute ~level ~rejector ~move storage =
    let committer = Tezos.sender in
    Move.execute ~level ~committer ~rejector ~player:Committer ~move storage
end
module Attack (Tezos : Tezos) = struct
  module Move = Move (Tezos)
  let requires = Move.requires

  (* O(log n) *)
  let execute ~level ~committer ~move storage =
    let rejector = Tezos.sender in
    Move.execute ~level ~committer ~rejector ~player:Rejector ~move storage
end

module Trust_commit (Tezos : Tezos) = struct
  (* TODO: why it requires something?*)
  let requires = Some Stake

  (* O(log n) *)
  let execute ~level ~committer storage =
    let current_turn = Turn.current ~current_level:Tezos.level ~level in

    let trusted_level = Storage.trusted_level storage in
    let trusted_state_hash = Storage.trusted_state_hash storage in

    let commit_data =
      match Storage.find_commit ~level ~committer storage with
      | Some commit_data -> commit_data
      | None -> failwith "unknown committer" in
    let previous_state_hash =
      Storage.Commit_data.previous_state_hash commit_data in

    let () = assert (Turn.can_trust_commit ~current_turn) in
    let () = assert (level = trusted_level + [%nat 1]) in
    let () = assert (previous_state_hash = trusted_state_hash) in
    let () = assert (Storage.Commit_data.games commit_data = [%nat 0]) in

    let storage =
      match Storage.remove_level ~level storage with
      | Some storage -> storage
      | None -> failwith "unreachable" in
    let storage = Storage.trust_level ~level storage in
    let storage = Storage.trust_state_hash previous_state_hash storage in

    (nil_operations, storage)
end
