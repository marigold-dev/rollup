open Environment
open Turn
open Forkable_rejection_game

type state = {
  last_turn : Turn.t;
  state : Forkable_rejection_game.state;
}
type t = state

type move =
  | Move_handshake      of { final_state_hash : state_hash }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay         of { vm_state : Vm.t }
  | Move_timeout

type move_result =
  | Move_result_winner  of player
  | Move_result_waiting of state
  | Move_result_invalid

let play ~current_turn ~previous_state_hash ~committer_steps
    ~committer_state_hash ~rejector_steps =
  match kind current_turn with
  | Turn_rejector ->
    let state =
      play ~previous_state_hash ~committer_steps ~committer_state_hash
        ~rejector_steps in
    Some { last_turn = current_turn; state }
  | _ -> None

type move_by_kind =
  | Timeout
  | Simple  of Simple_rejection_game.move

let simple_move ~current_turn player move state =
  match Forkable_rejection_game.move player move state with
  | Move_result_winner player -> Move_result_winner player
  | Move_result_waiting state ->
    Move_result_waiting { last_turn = current_turn; state }
  | Move_result_invalid -> Move_result_invalid
let move ~current_turn player move state =
  let { last_turn; state } = state in
  let can_move =
    (* allows moves right after creating / forking
       TODO: is it okay to allow moves right after creating? *)
    last_turn = current_turn
    ||
    match (kind current_turn, player) with
    | Turn_committer, Committer -> true
    | Turn_rejector, Rejector -> true
    | _ -> false in
  if can_move then
    let move =
      match move with
      | Move_handshake { final_state_hash } ->
        Simple (Move_handshake { final_state_hash })
      | Move_mid_state_hash { mid_state_hash } ->
        Simple (Move_mid_state_hash { mid_state_hash })
      | Move_replay { vm_state } -> Simple (Move_replay { vm_state })
      | Move_timeout -> Timeout in

    let must_timeout =
      last_turn <> current_turn
      &&
      (* C -> _ -> C | R -> _ -> R*)
      match (kind last_turn, player) with
      | Turn_committer, Committer -> true
      | Turn_rejector, Rejector -> true
      | _ -> false in
    match (must_timeout, move) with
    | true, Timeout -> Move_result_winner player
    | true, _
    | false, Timeout ->
      Move_result_invalid
    | _, Simple move -> simple_move ~current_turn player move state
  else
    Move_result_invalid

let fork ~current_turn ~as_ state =
  let { last_turn; state } = state in

  match (as_, kind current_turn) with
  | Committer, Turn_fork_committer
  | Rejector, Turn_fork_rejector -> (
    let forked_state =
      match (as_, kind last_turn) with
      | Committer, Turn_committer
      | Rejector, Turn_rejector ->
        fork state
      | _ ->
        (* TODO: big difference between forking when the player did not move *)
        (* TODO: test this, essentially fork when timeout is different *)
        Some state in
    match forked_state with
    | Some state -> Some { last_turn = current_turn; state }
    | None -> None)
  | _ -> None
