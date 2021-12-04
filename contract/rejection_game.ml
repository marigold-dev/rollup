open Environment

type vote = Agree | Disagree

module Small_rejection_game : sig
  type state

  type action =
    | Send_hash of state_hash
    (* TODO: vote *)
    | Vote of vote
    | Replay of Vm.t

  val start :
    previous_state_hash:state_hash ->
    committer:state_hash * steps ->
    rejector:state_hash * steps ->
    mid_state_hash:state_hash ->
    state
  val play : player -> action -> state -> state
  val has_winner : state -> player option
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

open Small_rejection_game
type nonrec state = {
  (* duplicated *)
  level : level;
  last_turn : Turn.t;
  (* state *)
  previous_state : state option;
  current_state : state;
}
type t = state

type defend = Vote of vote | Timeout
type attack = Mid_hash of state_hash | Replay of Vm.t | Timeout

let make ~level state =
  let current_turn = Turn.current ~level in
  {
    level;
    last_turn = current_turn;
    previous_state = None;
    current_state = state;
  }
let start ~level ~previous_state_hash ~committer ~rejector ~mid_state_hash =
  let state = start ~previous_state_hash ~committer ~rejector ~mid_state_hash in
  make ~level state

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

let assert_can_claim_timeout player state =
  let current_turn = current_turn state in
  (* TODO: duplicatead Turn.current *)
  let () = assert_is_expected_player player state in
  assert (current_turn = state.last_turn + turns_per_round)

let play player action state =
  let { level; last_turn; previous_state = _; current_state } = state in
  let previous_state = Some current_state in
  let current_state = play player action current_state in
  { level; last_turn; previous_state; current_state }

type move_result = Winner of player | Waiting of state

let defend move state =
  match move with
  | Vote vote -> Waiting (play Committer (Vote vote) state)
  | Timeout ->
      let () = assert_can_claim_timeout Committer state in
      Winner Committer
let attack move state =
  match move with
  | Mid_hash hash -> Waiting (play Rejector (Send_hash hash) state)
  | Replay vm_state -> (
      let state = play Rejector (Replay vm_state) state in
      match has_winner state.current_state with
      | Some player -> Winner player
      | None -> assert false)
  | Timeout ->
      let () = assert_can_claim_timeout Rejector state in
      Winner Rejector

let fork state =
  (* TODO: do we care about last_turn *)
  let { level; last_turn = _; previous_state; current_state = _ } = state in
  let () = assert (Turn.turn_kind ~level = Fork_rejector) in

  match previous_state with
  | Some state -> Some (make ~level state)
  | None -> None
