open Environment

(* TODO: ensure that only a single valid movement
         can be done for each player at each stage *)
(* TODO: can we abstract this types away while keeping the tests?*)
type search_state = private {
  initial_step : steps;
  initial_state_hash : state_hash;
  final_step : steps;
  final_state_hash : state_hash;
}
type handshake_state = private {
  initial_state_hash : state_hash;
  final_step : steps;
}
type searching_state = private {
  search_state : search_state;
  committer_mid_state_hash : state_hash option;
  rejector_mid_state_hash : state_hash option;
}
type replay_state = private {
  base_state_hash : state_hash;
  committer_state_hash : state_hash;
}
type state = private
  | Handshake of handshake_state
  | Searching of searching_state
  | Replay    of replay_state
type t = state

type move =
  | Move_handshake      of { final_state_hash : state_hash }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay         of { vm_state : Vm.t }
type move_result = private
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
