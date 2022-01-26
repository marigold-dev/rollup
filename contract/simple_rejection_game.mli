open Environment

type state
type t = state

type move =
  | Move_handshake      of { final_state_hash : state_hash }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay         of { vm_state : Vm.t }
type move_result =
  | Move_result_winner  of player
  | Move_result_waiting of state
  | Move_result_invalid

val play :
  previous_state_hash:state_hash ->
  committer_steps:steps ->
  committer_state_hash:state_hash ->
  rejector_steps:steps ->
  t

val move : player -> move -> state -> move_result

(* used by the forkable game *)
type search_state = private {
  initial_step : steps;
  initial_state_hash : state_hash;
  final_step : steps;
  final_state_hash : state_hash;
}
val search_state : state -> search_state option
