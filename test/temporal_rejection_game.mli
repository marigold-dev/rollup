open Environment

type state
type t = state

type move =
  | Move_handshake      of { final_state_hash : state_hash }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay         of { vm_state : Vm.t }
  | Move_timeout

type move_result = private
  | Move_result_winner  of player
  | Move_result_waiting of state
  | Move_result_invalid

val play :
  current_turn:Turn.t ->
  previous_state_hash:state_hash ->
  committer_steps:steps ->
  committer_state_hash:state_hash ->
  rejector_steps:steps ->
  t option

val move : current_turn:Turn.t -> player -> move -> state -> move_result
val fork : current_turn:Turn.t -> as_:player -> state -> state option
