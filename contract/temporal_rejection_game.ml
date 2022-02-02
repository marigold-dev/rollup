open Environment
open Turn
open Forkable_rejection_game

(* TODO: write down edge cases

   - timeout right after forking
   - doing a move and claiming timeout in the same turn
*)
type state = {
  last_committer_turn : Turn.t;
  last_rejector_turn : Turn.t;
  state : Forkable_rejection_game.state;
}

type t = state

type move =
  | Move_handshake      of { final_state_hash : state_hash }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay         of { vm_state : Vm.t }
  | Move_timeout

type move_result =
  | Move_result_winner        of player
  | Move_result_waiting       of state
  | Move_result_not_your_turn
  | Move_result_invalid

let play ~current_turn ~initial_state_hash ~committer_steps ~rejector_steps =
  if current_turn = Turn.first_rejector_turn then
    let state = play ~initial_state_hash ~committer_steps ~rejector_steps in
    Some
      {
        last_committer_turn = Turn.first_committer_turn;
        last_rejector_turn = Turn.first_rejector_turn;
        state;
      }
  else
    None

type move_by_kind =
  | Timeout
  | Simple  of Simple_rejection_game.move

let simple_move ~next_last_committer_turn ~next_last_rejector_turn player move
    state =
  match Forkable_rejection_game.move player move state with
  | Move_result_winner player -> Move_result_winner player
  | Move_result_waiting forkable_state ->
    Move_result_waiting
      {
        last_committer_turn = next_last_committer_turn;
        last_rejector_turn = next_last_rejector_turn;
        state = forkable_state;
      }
  | Move_result_invalid -> Move_result_invalid

let do_move ~current_turn player move state =
  let { last_committer_turn; last_rejector_turn; state } = state in
  let move =
    match move with
    | Move_handshake { final_state_hash } ->
      Simple (Move_handshake { final_state_hash })
    | Move_mid_state_hash { mid_state_hash } ->
      Simple (Move_mid_state_hash { mid_state_hash })
    | Move_replay { vm_state } -> Simple (Move_replay { vm_state })
    | Move_timeout -> Timeout in

  let last_opposite_player_turn =
    match player with
    | Committer -> last_rejector_turn
    | Rejector -> last_committer_turn in
  let must_timeout =
    Turn.must_timeout ~last_opposite_player_turn ~current_turn in
  match (must_timeout, move) with
  | true, Timeout -> Move_result_winner player
  | true, Simple _
  | false, Timeout ->
    Move_result_invalid
  | false, Simple move ->
    let next_last_committer_turn, next_last_rejector_turn =
      match player with
      | Committer -> (current_turn, last_rejector_turn)
      | Rejector -> (last_committer_turn, current_turn) in
    simple_move ~next_last_committer_turn ~next_last_rejector_turn player move
      state

let move ~current_turn player move state =
  (* TODO: is it okay to allow moves right after creating? *)
  match (kind current_turn, player) with
  | Turn_committer, Committer
  | Turn_rejector, Rejector ->
    do_move ~current_turn player move state
  | _ -> Move_result_not_your_turn

let do_fork_and_move ~current_turn player move state =
  let { last_committer_turn; last_rejector_turn; state } = state in
  let forked_state =
    let last_player_turn =
      match player with
      | Committer -> last_committer_turn
      | Rejector -> last_rejector_turn in
    if Turn.should_undo_move ~last_player_turn ~current_turn then
      fork state
    else
      Some state in

  match forked_state with
  | Some state ->
    let initial_state = { last_committer_turn; last_rejector_turn; state } in
    Some (do_move ~current_turn player move initial_state)
  | None -> None

let fork_and_move ~current_turn player move state =
  match (player, kind current_turn) with
  | Committer, Turn_fork_committer
  | Rejector, Turn_fork_rejector ->
    do_fork_and_move ~current_turn player move state
  | _ -> None
