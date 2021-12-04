open Environment

type player = Rejector | Committer

module Small_rejection_game : sig
  type state

  type vote = Agree | Disagree
  type action =
    | Send_hash of state_hash
    (* TODO: vote *)
    | Vote of vote
    | Replay of Vm.t

  val play : player -> action -> state -> state
end = struct
  (* TODO: if we require commit to include steps we can skip a couple turns *)
  (* TODO: if we require the committer to send the hash, we can fuse turns *)
  (* TODO: remove committer if it looses as a rejector *)
  (* TODO: log2 of steps to know maximum of turns instead of holding each individual step, thank you Daniel*)

  type state =
    | Vote_on_midpoint of {
        initial : state_hash * steps;
        mid : state_hash * steps;
        final : state_hash * steps * player;
      }
    | Waiting_midpoint of {
        initial : state_hash * steps;
        final : state_hash * steps * player;
      }
    | To_replay of { state_hash : state_hash; expected : state_hash * player }
    | Winner of player

  type vote = Agree | Disagree
  type action = Send_hash of state_hash | Vote of vote | Replay of Vm.t

  (* TODO: a game can only have a single fork per level *)
  (* let find_game : game_id -> game = assert false *)

  let calculate_mid_step ~initial_step ~final_step =
    let diff = abs (final_step - initial_step) in
    match diff / 2n with
    | Some (mid_step, _remainder) -> mid_step
    | None -> assert false

  let start ~previous_state_hash ~committer ~rejector ~mid_state_hash =
    let committer_state_hash, committer_steps = committer in
    let rejector_state_hash, rejector_steps = rejector in
    let final =
      if committer_steps > rejector_steps then
        (rejector_state_hash, rejector_steps, Rejector)
      else (committer_state_hash, committer_steps, Committer)
    in
    let initial_step = 0n in
    let initial = (previous_state_hash, initial_step) in
    let _final_hash, final_step, _final_player = final in

    (* WARNING: *)
    (* VM steps needs to be bigger than 2n*)
    (* TODO: slash instead of fail *)
    let () = assert (final_step >= 2n) in
    let mid =
      let mid_step = calculate_mid_step ~initial_step ~final_step in
      (mid_state_hash, mid_step)
    in
    Vote_on_midpoint { initial; mid; final }

  (* TODO: proof showing that committer always agrees with initial hash *)
  (* TODO: proof showing that committer almost always disagrees with final hash *)

  let has_winner state =
    match state with Winner player -> Some player | _ -> None

  let play player action state =
    match (state, player, action) with
    | Waiting_midpoint { initial; final }, Rejector, Send_hash mid_hash ->
        let _initial_hash, initial_step = initial in
        let _final_hash, final_step, _expecting = final in
        let mid_step = calculate_mid_step ~initial_step ~final_step in
        let mid = (mid_hash, mid_step) in
        Vote_on_midpoint { initial; mid; final }
    | Vote_on_midpoint { initial; mid; final }, Committer, Vote vote ->
        let initial, final =
          match vote with
          | Agree -> (mid, final)
          | Disagree ->
              let mid_state_hash, mid_step = mid in
              let final = (mid_state_hash, mid_step, Rejector) in
              (initial, final)
        in

        let initial_hash, initial_step = initial in
        let final_hash, final_step, expecting = final in
        if initial_step + 1n = final_step then
          To_replay
            { state_hash = initial_hash; expected = (final_hash, expecting) }
        else Waiting_midpoint { initial; final }
    | To_replay { state_hash; expected }, Rejector, Replay state ->
        (* TODO: slash instead of fail *)
        let () = assert (Vm.hash state = state_hash) in
        let state' = Vm.execute_step state in

        let expected_hash, expected_author = expected in
        let opposite_player = function
          | Rejector -> Committer
          | Committer -> Rejector
        in
        let winner =
          if Vm.hash state' = expected_hash then expected_author
          else opposite_player expected_author
        in

        Winner winner
    | _ ->
        (* TODO: slash instead of fail *)
        assert false
end

type state
type t = state

module Large_rejection_game : sig
  open Small_rejection_game

  type state

  val play : player -> action -> state -> state
  val timeout : player -> state -> state
  (* val fork : player -> action -> state -> state *)
end = struct
  open Small_rejection_game
  type nonrec state = {
    level : level;
    last_turn : Turn.t;
    previous_player : player;
    previous_state : state;
    previous_action : action;
    current_state : state;
  }

  let half_turns_per_round = 2n
  let turns_per_round = 4n

  let current_turn state = Turn.current ~level:state.level
  let expected_player state =
    (* C -> W -> R -> W *)
    match current_turn state / turns_per_round with
    | Some (_, remainder) ->
        if remainder = 0n then Some Committer
        else if remainder = 2n then Some Rejector
        else None
    | None -> assert false

  let assert_is_expected_player player state =
    (* TODO: slash instead of fail *)
    match expected_player state with
    | Some expected_player when player = expected_player -> ()
    | Some _ -> failwith "not your turn"
    | None -> failwith "waiting period"

  (* timeout() -> remove_game | remove_commit *)

  let can_claim_timeout ~player state =
    let current_turn = current_turn state in
    match player with
    | Some player ->
        (* TODO: duplicatead Turn.current *)
        let () = assert_is_expected_player player state in
        current_turn = state.last_turn + half_turns_per_round
    | None -> current_turn > state.last_turn + turns_per_round

  (* C -> W -> R -> W -> H *)

  let play player action state =
    let () = assert_is_expected_player player state in
    (* TODO: duplicated assert_is_expected_player *)
    let () = assert (not (can_claim_timeout ~player:(Some player) state)) in

    state

  let timeout player = assert false

  (* C -> W -> !R -> W -> C

     you can claim a timeout if you're the expected player and the last movement was made by you
  *)
  let fork player action state =
    let { previous_state; previous_action; current_state = _; _ } = state in
    let () =
      (* TODO: slash instead of fail *)
      match (state.previous_action, action) with
      | Send_hash previous_hash, Send_hash alternative_hash
        when previous_hash <> alternative_hash ->
          ()
      | Replay _, Replay _ ->
          (* TODO: maybe if we slash this can be the case??? *)
          failwith "cannot fork a replay, same result"
      | _ -> failwith "can only fork on send hash"
    in
    state

  let claim_victory : state -> player option = assert false
end

let fork = assert false
type turn_kind = Committer | Fork_committer | Rejector | Fork_rejector
let turn_kind = assert false
let start = assert false
