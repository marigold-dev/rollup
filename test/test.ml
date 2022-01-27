open Environment
open Simple_rejection_game

(* utils *)
let ediv_2 n =
  match ediv n (nat 2) with
  | Some (div, rem) -> (div, rem)
  | None -> assert false
let mid_step ~initial_step ~final_step =
  let diff = abs (final_step - initial_step) in
  let offset, _rem = ediv_2 diff in
  initial_step + offset

(* TODO: test this with something like qcheck?*)
let () = assert (mid_step ~initial_step:(nat 0) ~final_step:(nat 5) = nat 2)
let () = assert (mid_step ~initial_step:(nat 3) ~final_step:(nat 5) = nat 4)
let () = assert (mid_step ~initial_step:(nat 3) ~final_step:(nat 7) = nat 5)
let () = assert (mid_step ~initial_step:(nat 6) ~final_step:(nat 9) = nat 7)

let random_hash base =
  let seed = Cstruct.to_string (Mirage_crypto_rng.generate 32) in
  Crypto.blake2b (Bytes.of_string (base ^ seed))
let committer_hash () = random_hash "committer_"
let rejector_hash () = random_hash "rejector_"
let invalid_hash () = random_hash "invalid_"
let steps () =
  let seed = Cstruct.to_bytes (Mirage_crypto_rng.generate 1) in
  (* TODO: try bigger numbers *)
  (* TODO: try entire range in a run, like, 0-32 *)
  (* up to 32 *)
  let int = Stdlib.Bytes.get_uint8 seed 0 lsr 3 in
  nat int + nat 2
let meaningless name = failwith ("meaningless " ^ name)

(* framework *)
type expected_state =
  | Expected_state_handshake         of {
      expected_initial_state_hash : state_hash;
      expected_final_step : steps;
    }
  | Expected_state_searching         of {
      expected_initial_step : steps;
      expected_initial_state_hash : state_hash;
      expected_final_step : steps;
      expected_final_state_hash : state_hash;
    }
  | Expected_state_waiting_committer of {
      expected_rejector_mid_state_hash : state_hash;
    }
  | Expected_state_waiting_rejector  of {
      expected_committer_mid_state_hash : state_hash;
    }
  | Expected_state_replay            of {
      expected_base_state_hash : state_hash;
      expected_committer_state_hash : state_hash;
    }
  | Expected_state_noop

type expected_move_result =
  | Expected_move_result_winner  of player
  | Expected_move_result_waiting of expected_state
  | Expected_move_result_invalid

type case_play = {
  state : state;
  expected_state : expected_state;
}
type case_move = {
  move_result : move_result;
  expected_move_result : expected_move_result;
}

let check_state state =
  (* TODO: guarantee that all the hashes are agreed upon by the committer *)
  match state with
  | Handshake { initial_state_hash = _; final_step } -> final_step >= nat 2
  | Searching
      {
        search_state =
          {
            initial_step;
            initial_state_hash = _;
            final_step;
            final_state_hash = _;
          };
        committer_mid_state_hash;
        rejector_mid_state_hash;
      } ->
    let valid_configuration =
      Option.is_none committer_mid_state_hash
      || Option.is_none rejector_mid_state_hash in
    let valid_diff =
      let diff = final_step - initial_step in
      (* TODO: polymorphic comparison*)
      Stdlib.(diff >= int 2) in
    valid_configuration && valid_diff
  | Replay { base_state_hash = _; committer_state_hash = _ } -> true

(* TODDO: check all = _ patterns*)
let check_state_transition ~previous state =
  match (previous, state) with
  | ( Handshake
        {
          initial_state_hash = handshake_initial_state_hash;
          final_step = handshake_final_step;
        },
      Searching
        {
          search_state =
            {
              initial_step;
              initial_state_hash;
              final_step;
              final_state_hash = _;
            };
          committer_mid_state_hash;
          rejector_mid_state_hash;
        } ) ->
    Option.is_none committer_mid_state_hash
    && Option.is_none rejector_mid_state_hash
    && initial_step = nat 0
    && handshake_initial_state_hash = initial_state_hash
    && handshake_final_step = final_step
  | Handshake _, (Handshake _ | Replay _) -> false
  (* collecting mid state hash *)
  | ( Searching
        {
          search_state = previous_search_state;
          committer_mid_state_hash = None;
          rejector_mid_state_hash = None;
        },
      Searching
        { search_state; committer_mid_state_hash; rejector_mid_state_hash } ) ->
    previous_search_state = search_state
    && (Option.is_none committer_mid_state_hash
       || Option.is_none rejector_mid_state_hash)
    && (Option.is_some committer_mid_state_hash
       || Option.is_some rejector_mid_state_hash)
  (* noop *)
  | ( Searching
        {
          search_state = previous_search_state;
          committer_mid_state_hash = previous_committer_mid_state_hash;
          rejector_mid_state_hash = previous_rejector_mid_state_hash;
        },
      Searching
        { search_state; committer_mid_state_hash; rejector_mid_state_hash } )
    when previous_search_state = search_state ->
    previous_committer_mid_state_hash = committer_mid_state_hash
    && previous_rejector_mid_state_hash = rejector_mid_state_hash
  | ( Searching
        {
          search_state =
            {
              initial_step = previous_initial_step;
              initial_state_hash = previous_initial_state_hash;
              final_step = previous_final_step;
              final_state_hash = previous_final_state_hash;
            };
          committer_mid_state_hash = previous_committer_mid_state_hash;
          rejector_mid_state_hash = previous_rejector_mid_state_hash;
        },
      Searching
        {
          search_state =
            { initial_step; initial_state_hash; final_step; final_state_hash };
          committer_mid_state_hash;
          rejector_mid_state_hash;
        } ) ->
    let valid_previous_configuration =
      (Option.is_none previous_rejector_mid_state_hash
      || Option.is_none previous_committer_mid_state_hash)
      && (Option.is_some previous_rejector_mid_state_hash
         || Option.is_some previous_committer_mid_state_hash) in
    let valid_configuration =
      Option.is_none committer_mid_state_hash
      && Option.is_none rejector_mid_state_hash in

    let valid_hash_change =
      previous_initial_state_hash = initial_state_hash
      && previous_final_state_hash <> final_state_hash
      || previous_initial_state_hash <> initial_state_hash
         && previous_final_state_hash = final_state_hash in
    (* TODO: there is more properties based on if they agree when
              using previous_committer_mid_state_hash and previous_rejector_mid_state_hash *)
    let valid_initial_step = initial_step >= previous_initial_step in
    let valid_final_step = final_step <= previous_final_step in
    let valid_diff_change =
      let previous_diff = abs (previous_final_step - previous_initial_step) in
      let diff = abs (final_step - initial_step) in
      match ediv_2 previous_diff with
      | half_previous_diff, rem when rem = nat 0 -> half_previous_diff = diff
      | half_previous_diff, _rem ->
        half_previous_diff = diff || half_previous_diff + nat 1 = diff in

    valid_previous_configuration
    && valid_configuration
    && valid_hash_change
    && valid_initial_step
    && valid_final_step
    && valid_diff_change
  | ( Searching
        {
          search_state =
            {
              initial_step = previous_initial_step;
              initial_state_hash = previous_initial_state_hash;
              final_step = previous_final_step;
              final_state_hash = _;
            };
          committer_mid_state_hash = previous_committer_mid_state_hash;
          rejector_mid_state_hash = previous_rejector_mid_state_hash;
        },
      Replay { base_state_hash; committer_state_hash = _ } ) ->
    let valid_previous_configuration =
      (Option.is_none previous_rejector_mid_state_hash
      || Option.is_none previous_committer_mid_state_hash)
      && (Option.is_some previous_rejector_mid_state_hash
         || Option.is_some previous_committer_mid_state_hash) in

    (* TODO: there is more properties based on if they agree when
         using previous_committer_mid_state_hash and previous_rejector_mid_state_hash *)
    let valid_hash_diff =
      base_state_hash = previous_initial_state_hash
      ||
      match
        (previous_committer_mid_state_hash, previous_rejector_mid_state_hash)
      with
      | Some committer_mid_state_hash, None ->
        base_state_hash = committer_mid_state_hash
      | None, Some rejector_mid_state_hash ->
        base_state_hash = rejector_mid_state_hash
      | _ -> false in
    let valid_previous_diff =
      let diff = previous_final_step - previous_initial_step in
      diff = int 2 || diff = int 3 in
    valid_previous_configuration && valid_hash_diff && valid_previous_diff
  | Searching _, Handshake _ -> false
  | Replay _, (Searching _ | Handshake _ | Replay _) -> false

(* TODO: stop using polymorphic comparison? *)
let check_play expected_state state =
  match (expected_state, state) with
  | ( Expected_state_handshake
        { expected_initial_state_hash; expected_final_step },
      Handshake { initial_state_hash; final_step } ) ->
    expected_initial_state_hash = initial_state_hash
    && expected_final_step = final_step
  | Expected_state_handshake _, (Searching _ | Replay _) -> false
  | ( Expected_state_searching
        {
          expected_initial_step;
          expected_initial_state_hash;
          expected_final_step;
          expected_final_state_hash;
        },
      Searching
        {
          search_state =
            { initial_step; initial_state_hash; final_step; final_state_hash };
          committer_mid_state_hash = None;
          rejector_mid_state_hash = None;
        } ) ->
    expected_initial_step = initial_step
    && expected_initial_state_hash = initial_state_hash
    && expected_final_step = final_step
    && expected_final_state_hash = final_state_hash
  | Expected_state_searching _, (Searching _ | Handshake _ | Replay _) -> false
  (* TODO: maybe enforce those on the typer? *)
  | Expected_state_waiting_committer _, _ ->
    meaningless "Expected_state_waiting_committer"
  | Expected_state_waiting_rejector _, _ ->
    meaningless "Expected_state_waiting_rejector"
  | Expected_state_replay _, _ -> meaningless "Expected_replay"
  | Expected_state_noop, _ -> meaningless "Expected_state_noop"
let check_move ~previous expected_state state =
  match expected_state with
  (* TODO: maybe enforce this on the typer? *)
  | Expected_state_handshake _ -> meaningless "Expected_state_handshake"
  | Expected_state_searching
      {
        expected_initial_step;
        expected_initial_state_hash;
        expected_final_step;
        expected_final_state_hash;
      } -> (
    match state with
    | Searching
        {
          search_state =
            { initial_step; initial_state_hash; final_step; final_state_hash };
          committer_mid_state_hash = None;
          rejector_mid_state_hash = None;
        } ->
      expected_initial_step = initial_step
      && expected_initial_state_hash = initial_state_hash
      && expected_final_step = final_step
      && expected_final_state_hash = final_state_hash
    | Searching _
    | Handshake _
    | Replay _ ->
      false)
  | Expected_state_waiting_committer { expected_rejector_mid_state_hash } -> (
    match (previous, state) with
    | ( Searching
          {
            search_state = previous_search_state;
            committer_mid_state_hash = None;
            rejector_mid_state_hash = None;
          },
        Searching
          {
            search_state = current_search_state;
            committer_mid_state_hash = None;
            rejector_mid_state_hash = Some rejector_mid_state_hash;
          } ) ->
      previous_search_state = current_search_state
      && expected_rejector_mid_state_hash = rejector_mid_state_hash
    | _ -> false)
  | Expected_state_waiting_rejector { expected_committer_mid_state_hash } -> (
    match (previous, state) with
    | ( Searching
          {
            search_state = previous_search_state;
            committer_mid_state_hash = None;
            rejector_mid_state_hash = None;
          },
        Searching
          {
            search_state = current_search_state;
            committer_mid_state_hash = Some committer_mid_state_hash;
            rejector_mid_state_hash = None;
          } ) ->
      previous_search_state = current_search_state
      && expected_committer_mid_state_hash = committer_mid_state_hash
    | _ -> false)
  | Expected_state_replay
      { expected_base_state_hash; expected_committer_state_hash } -> (
    (* TODO: is this correct??? *)
    match state with
    | Replay { base_state_hash; committer_state_hash } ->
      expected_base_state_hash = base_state_hash
      && expected_committer_state_hash = committer_state_hash
    | _ -> false)
  | Expected_state_noop -> previous = state

let failed ~path reason =
  let path = Stdlib.List.rev path in
  failwith ("failed at " ^ String.concat " -> " path ^ ": " ^ reason)
let apply_move ~path previous_state move =
  let { move_result; expected_move_result } = move previous_state in
  match (expected_move_result, move_result) with
  | Expected_move_result_winner expected_winner, Move_result_winner winner ->
    if winner = expected_winner then
      None
    else
      failed ~path "wrong winner"
  | Expected_move_result_winner _, (Move_result_waiting _ | Move_result_invalid)
    ->
    failed ~path "Expected_move_result_winner"
  | Expected_move_result_waiting expected_state, Move_result_waiting state -> (
    if not (check_state state) then
      failed ~path "check_state";
    if not (check_state_transition ~previous:previous_state state) then
      failed ~path "check_state_transition";
    if not (check_move ~previous:previous_state expected_state state) then
      failed ~path "check_move";
    match expected_state with
    | Expected_state_noop -> None
    | _ -> Some state)
  | Expected_move_result_waiting _, (Move_result_winner _ | Move_result_invalid)
    ->
    failed ~path "Expected_move_result_waiting"
  | Expected_move_result_invalid, Move_result_invalid -> None
  | Expected_move_result_invalid, (Move_result_waiting _ | Move_result_winner _)
    ->
    failed ~path "Expected_move_result_invalid"
let rec apply_moves ~path state moves =
  let states =
    Stdlib.List.filter_map
      (fun (name, move) ->
        let path = name :: path in
        match apply_move ~path state move with
        | Some state -> Some (path, state)
        | None -> None)
      moves in
  Stdlib.List.iter (fun (path, state) -> apply_moves ~path state moves) states

let start_game ~path play =
  let { state; expected_state } = play () in
  if not (check_state state) then
    failed ~path "check_state";
  if not (check_play expected_state state) then
    failed ~path "check_play";
  state
let bootstrap seeds moves =
  let states =
    Stdlib.List.map
      (fun (name, play) ->
        let path = [name] in
        let state = start_game ~path play in
        (path, state))
      seeds in
  Stdlib.List.iter (fun (path, state) -> apply_moves ~path state moves) states

(* play *)
let agree_on_steps ~previous_state_hash ~steps =
  let state =
    play ~previous_state_hash ~committer_steps:steps ~rejector_steps:steps in
  let expected_state =
    Expected_state_handshake
      {
        expected_initial_state_hash = previous_state_hash;
        expected_final_step = steps;
      } in
  { state; expected_state }
let committer_steps_bigger ~previous_state_hash ~steps =
  let state =
    play ~previous_state_hash
      ~committer_steps:(steps + nat 1)
      ~rejector_steps:steps in
  let expected_state =
    Expected_state_handshake
      {
        expected_initial_state_hash = previous_state_hash;
        expected_final_step = steps;
      } in
  { state; expected_state }
let rejector_steps_bigger ~previous_state_hash ~steps =
  let state =
    play ~previous_state_hash ~committer_steps:steps
      ~rejector_steps:(steps + nat 1) in
  let expected_state =
    Expected_state_handshake
      {
        expected_initial_state_hash = previous_state_hash;
        expected_final_step = steps;
      } in
  { state; expected_state }

(* handshake *)
let committer_handshake ~committer_final_state_hash state =
  let move_result =
    move Committer
      (Move_handshake { final_state_hash = committer_final_state_hash })
      state in
  let expected_move_result =
    match state with
    | Handshake { initial_state_hash; final_step } ->
      let expected =
        Expected_state_searching
          {
            expected_initial_step = nat 0;
            expected_initial_state_hash = initial_state_hash;
            expected_final_step = final_step;
            expected_final_state_hash = committer_final_state_hash;
          } in
      Expected_move_result_waiting expected
    | Searching _
    | Replay _ ->
      Expected_move_result_invalid in
  { move_result; expected_move_result }
let rejector_handshake ~rejector_final_state_hash state =
  let move_result =
    move Rejector
      (Move_handshake { final_state_hash = rejector_final_state_hash })
      state in
  let expected_move_result = Expected_move_result_invalid in
  { move_result; expected_move_result }

(* mid_state_hash *)
let committer_agrees ~committer_mid_state_hash state =
  let committer_mid_state_hash, expected_move_result =
    match state with
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = Some committer_mid_state_hash;
          rejector_mid_state_hash = None;
        } ->
      let expected_state = Expected_state_noop in
      (committer_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = None;
          rejector_mid_state_hash = None;
        } ->
      let expected_state =
        Expected_state_waiting_rejector
          { expected_committer_mid_state_hash = committer_mid_state_hash } in
      (committer_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state =
            {
              initial_step;
              initial_state_hash = _;
              final_step;
              final_state_hash;
            };
          committer_mid_state_hash = _;
          rejector_mid_state_hash = Some rejector_mid_state_hash;
        } ->
      let initial_step = mid_step ~initial_step ~final_step in

      let expected_state =
        if initial_step + nat 1 = final_step then
          Expected_state_replay
            {
              expected_base_state_hash = rejector_mid_state_hash;
              expected_committer_state_hash = final_state_hash;
            }
        else
          Expected_state_searching
            {
              expected_initial_step = initial_step;
              expected_initial_state_hash = rejector_mid_state_hash;
              expected_final_step = final_step;
              expected_final_state_hash = final_state_hash;
            } in
      (rejector_mid_state_hash, Expected_move_result_waiting expected_state)
    | Handshake _
    | Replay _ ->
      (committer_mid_state_hash, Expected_move_result_invalid) in
  let move_result =
    move Committer
      (Move_mid_state_hash { mid_state_hash = committer_mid_state_hash })
      state in
  { move_result; expected_move_result }
let rejector_agrees ~rejector_mid_state_hash state =
  let rejector_mid_state_hash, expected_move_result =
    match state with
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = None;
          rejector_mid_state_hash = Some rejector_mid_state_hash;
        } ->
      let expected_state = Expected_state_noop in
      (rejector_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = None;
          rejector_mid_state_hash = None;
        } ->
      let expected_state =
        Expected_state_waiting_committer
          { expected_rejector_mid_state_hash = rejector_mid_state_hash } in
      (rejector_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state =
            {
              initial_step;
              initial_state_hash = _;
              final_step;
              final_state_hash;
            };
          committer_mid_state_hash = Some committer_mid_state_hash;
          rejector_mid_state_hash = _;
        } ->
      let initial_step = mid_step ~initial_step ~final_step in

      let expected_state =
        if initial_step + nat 1 = final_step then
          Expected_state_replay
            {
              expected_base_state_hash = committer_mid_state_hash;
              expected_committer_state_hash = final_state_hash;
            }
        else
          Expected_state_searching
            {
              expected_initial_step = initial_step;
              expected_initial_state_hash = committer_mid_state_hash;
              expected_final_step = final_step;
              expected_final_state_hash = final_state_hash;
            } in
      (committer_mid_state_hash, Expected_move_result_waiting expected_state)
    | Handshake _
    | Replay _ ->
      (rejector_mid_state_hash, Expected_move_result_invalid) in
  let move_result =
    move Rejector
      (Move_mid_state_hash { mid_state_hash = rejector_mid_state_hash })
      state in
  { move_result; expected_move_result }

let committer_disagrees ~committer_mid_state_hash state =
  let committer_mid_state_hash, expected_move_result =
    match state with
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = Some committer_mid_state_hash;
          rejector_mid_state_hash = None;
        } ->
      let expected_state = Expected_state_noop in
      (committer_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = None;
          rejector_mid_state_hash = None;
        } ->
      let expected_state =
        Expected_state_waiting_rejector
          { expected_committer_mid_state_hash = committer_mid_state_hash } in
      (committer_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state =
            {
              initial_step;
              initial_state_hash;
              final_step;
              final_state_hash = _;
            };
          committer_mid_state_hash = _;
          rejector_mid_state_hash = Some _rejector_mid_state_hash;
        } ->
      let final_step = mid_step ~initial_step ~final_step in

      let expected_state =
        if initial_step + nat 1 = final_step then
          Expected_state_replay
            {
              expected_base_state_hash = initial_state_hash;
              expected_committer_state_hash = committer_mid_state_hash;
            }
        else
          Expected_state_searching
            {
              expected_initial_step = initial_step;
              expected_initial_state_hash = initial_state_hash;
              expected_final_step = final_step;
              expected_final_state_hash = committer_mid_state_hash;
            } in
      (committer_mid_state_hash, Expected_move_result_waiting expected_state)
    | Handshake _
    | Replay _ ->
      (committer_mid_state_hash, Expected_move_result_invalid) in
  let move_result =
    move Committer
      (Move_mid_state_hash { mid_state_hash = committer_mid_state_hash })
      state in
  { move_result; expected_move_result }
let rejector_disagrees ~rejector_mid_state_hash state =
  let rejector_mid_state_hash, expected_move_result =
    match state with
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = None;
          rejector_mid_state_hash = Some rejector_mid_state_hash;
        } ->
      let expected_state = Expected_state_noop in
      (rejector_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state = _;
          committer_mid_state_hash = None;
          rejector_mid_state_hash = None;
        } ->
      let expected_state =
        Expected_state_waiting_committer
          { expected_rejector_mid_state_hash = rejector_mid_state_hash } in
      (rejector_mid_state_hash, Expected_move_result_waiting expected_state)
    | Searching
        {
          search_state =
            {
              initial_step;
              initial_state_hash;
              final_step;
              final_state_hash = _;
            };
          committer_mid_state_hash = Some committer_mid_state_hash;
          rejector_mid_state_hash = _;
        } ->
      let final_step = mid_step ~initial_step ~final_step in
      let expected_state =
        if initial_step + nat 1 = final_step then
          Expected_state_replay
            {
              expected_base_state_hash = initial_state_hash;
              expected_committer_state_hash = committer_mid_state_hash;
            }
        else
          Expected_state_searching
            {
              expected_initial_step = initial_step;
              expected_initial_state_hash = initial_state_hash;
              expected_final_step = final_step;
              expected_final_state_hash = committer_mid_state_hash;
            } in
      (rejector_mid_state_hash, Expected_move_result_waiting expected_state)
    | Handshake _
    | Replay _ ->
      (rejector_mid_state_hash, Expected_move_result_invalid) in
  let move_result =
    move Rejector
      (Move_mid_state_hash { mid_state_hash = rejector_mid_state_hash })
      state in
  { move_result; expected_move_result }

(* replay *)
let player_replay_committer_wins ~base_state_hash ~committer_state_hash player
    state =
  let base_state_hash, committer_state_hash, expected_move_result =
    match state with
    | Replay { base_state_hash; committer_state_hash } ->
      let expected_move_result = Expected_move_result_winner Committer in
      (base_state_hash, committer_state_hash, expected_move_result)
    | Handshake _
    | Searching _ ->
      (base_state_hash, committer_state_hash, Expected_move_result_invalid)
  in
  let vm_state = Vm.make ~hashes:[base_state_hash; committer_state_hash] in
  let vm_state =
    match Vm.single_step_data vm_state with
    | Some vm_state -> vm_state
    | None -> assert false in
  let move_result = move player (Move_replay { vm_state }) state in
  { move_result; expected_move_result }
let committer_replay_committer_wins ~base_state_hash ~committer_state_hash state
    =
  player_replay_committer_wins Committer ~base_state_hash ~committer_state_hash
    state
let rejector_replay_committer_wins ~base_state_hash ~committer_state_hash state
    =
  player_replay_committer_wins Rejector ~base_state_hash ~committer_state_hash
    state

let player_replay_rejector_wins ~base_state_hash ~random_state_hash player state
    =
  let base_state_hash, expected_move_result =
    match state with
    | Replay { base_state_hash; committer_state_hash = _ } ->
      let expected_move_result = Expected_move_result_winner Rejector in
      (base_state_hash, expected_move_result)
    | Handshake _
    | Searching _ ->
      (base_state_hash, Expected_move_result_invalid) in
  let vm_state = Vm.make ~hashes:[base_state_hash; random_state_hash] in
  let vm_state =
    match Vm.single_step_data vm_state with
    | Some vm_state -> vm_state
    | None -> assert false in
  let move_result = move player (Move_replay { vm_state }) state in
  { move_result; expected_move_result }
let committer_replay_rejector_wins ~base_state_hash ~random_state_hash state =
  player_replay_rejector_wins Committer ~base_state_hash ~random_state_hash
    state
let rejector_replay_rejector_wins ~base_state_hash ~random_state_hash state =
  player_replay_rejector_wins Rejector ~base_state_hash ~random_state_hash state

let player_replay_invalid player ~invalid_state_hash state =
  let vm_state = Vm.make ~hashes:[invalid_state_hash] in
  let vm_state =
    match Vm.single_step_data vm_state with
    | Some vm_state -> vm_state
    | None -> assert false in
  let move_result = move player (Move_replay { vm_state }) state in
  let expected_move_result = Expected_move_result_invalid in
  { move_result; expected_move_result }

let committer_replay_invalid ~invalid_state_hash state =
  player_replay_invalid Committer ~invalid_state_hash state
let rejector_replay_invalid ~invalid_state_hash state =
  player_replay_invalid Rejector ~invalid_state_hash state

let seed f () = f ~previous_state_hash:(committer_hash ()) ~steps:(steps ())
let seeds =
  [
    ("agree_on_steps", seed agree_on_steps);
    ("committer_steps_bigger", seed committer_steps_bigger);
    ("rejector_steps_bigger", seed rejector_steps_bigger);
  ]

let moves =
  [
    ( "committer_handshake",
      fun state ->
        committer_handshake ~committer_final_state_hash:(committer_hash ())
          state );
    ( "rejector_handshake",
      fun state ->
        rejector_handshake ~rejector_final_state_hash:(rejector_hash ()) state
    );
    ( "committer_agrees",
      fun state ->
        committer_agrees ~committer_mid_state_hash:(committer_hash ()) state );
    ( "rejector_agrees",
      fun state ->
        rejector_agrees ~rejector_mid_state_hash:(rejector_hash ()) state );
    ( "committer_disagrees",
      fun state ->
        committer_disagrees ~committer_mid_state_hash:(committer_hash ()) state
    );
    ( "rejector_disagrees",
      fun state ->
        rejector_disagrees ~rejector_mid_state_hash:(rejector_hash ()) state );
    ( "committer_replay_committer_wins",
      fun state ->
        committer_replay_committer_wins ~base_state_hash:(committer_hash ())
          ~committer_state_hash:(committer_hash ()) state );
    ( "rejector_replay_committer_wins",
      fun state ->
        rejector_replay_committer_wins ~base_state_hash:(committer_hash ())
          ~committer_state_hash:(committer_hash ()) state );
    ( "committer_replay_rejector_wins",
      fun state ->
        committer_replay_rejector_wins ~base_state_hash:(committer_hash ())
          ~random_state_hash:(invalid_hash ()) state );
    ( "rejector_replay_rejector_wins",
      fun state ->
        rejector_replay_rejector_wins ~base_state_hash:(committer_hash ())
          ~random_state_hash:(invalid_hash ()) state );
    ( "committer_replay_invalid",
      fun state ->
        committer_replay_invalid ~invalid_state_hash:(invalid_hash ()) state );
    ( "rejector_replay_invalid",
      fun state ->
        rejector_replay_invalid ~invalid_state_hash:(invalid_hash ()) state );
  ]

let () = Printexc.record_backtrace true
let () = bootstrap seeds moves
