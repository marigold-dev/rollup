open Environment

type state
type t = state

type move =
  | Move_handshake      of { final_state_hash : state_hash }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay         of { vm_state : Vm.t }
  | Move_timeout

type move_result = private
  | Move_result_winner        of player
  | Move_result_waiting       of state
  (* TODO: should we slash on invalid move? *)
  | Move_result_not_your_turn
  | Move_result_invalid

val play :
  current_turn:Turn.t ->
  initial_state_hash:state_hash ->
  committer_steps:steps ->
  rejector_steps:steps ->
  t option

val move : current_turn:Turn.t -> player -> move -> state -> move_result

val fork_and_move :
  current_turn:Turn.t -> player -> move -> state -> move_result option
