(* TODO: this only works if committer and rejector plays in different turns,
   because forking only undo the last movement *)
open Environment
open Simple_rejection_game

type state = private {
  previous_state : Simple_rejection_game.state option;
  state : Simple_rejection_game.state;
}
type t = state

type move_result = private
  | Move_result_winner  of player
  | Move_result_waiting of state
  | Move_result_invalid

val play :
  previous_state_hash:state_hash ->
  committer_steps:steps ->
  rejector_steps:steps ->
  t

val move : player -> move -> state -> move_result

(* TODO: this actually only undo the last movement *)
val fork : state -> state option
