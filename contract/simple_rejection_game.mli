open Environment

(* TODO: ensure that only a single valid movement
         can be done for each player at each stage *)
(* TODO: can we abstract this types away while keeping the tests?*)
type search_state = private {
  initial_step : Step.t;
  initial_state_hash : state_hash;
  final_step : Step.t;
  final_state_hash : state_hash;
}
type handshake_state = private {
  initial_state_hash : state_hash;
  final_step : Step.t;
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
  | Move_handshake      of {
      input_hash : input_hash;
      storage_hash : storage_hash;
    }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay         of { vm_state : Vm.t }
type move_result = private
  | Move_result_winner  of player
  | Move_result_waiting of state
  | Move_result_invalid

val play :
  initial_state_hash:state_hash ->
  committer_steps:Step.non_zero ->
  rejector_steps:Step.non_zero ->
  t

val move : player -> move -> state -> move_result
