open Environment

type state = {
  previous_state : Simple_rejection_game.state option;
  state : Simple_rejection_game.state;
}
type t = state

type move_result =
  | Move_result_winner  of player
  | Move_result_waiting of state
  | Move_result_invalid

let play ~previous_state_hash ~committer_steps ~committer_state_hash
    ~rejector_steps =
  let simple_rejection_game_state =
    Simple_rejection_game.play ~previous_state_hash ~committer_steps
      ~committer_state_hash ~rejector_steps in
  { previous_state = None; state = simple_rejection_game_state }

let move player move state =
  let { previous_state = _; state = previous_state } = state in
  match Simple_rejection_game.move player move previous_state with
  | Move_result_winner player -> Move_result_winner player
  | Move_result_waiting state ->
    Move_result_waiting { previous_state = Some previous_state; state }
  | Move_result_invalid -> Move_result_invalid

let fork state =
  (* TODO: forking rejection game where both players play at same time *)
  (* TODO: ensure that forking only can be used during searching *)
  (* TODO: test, forking as committer but last move was done by the rejector *)
  (* TODO: test, cannot fork twice in a row, why?  *)
  let { previous_state; state = _ } = state in
  match previous_state with
  | None -> None
  | Some state -> Some { previous_state = None; state }
