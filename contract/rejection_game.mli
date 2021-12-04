open Environment

type state
type t = state

type vote = Agree | Disagree
type defend = Vote of vote | Timeout

type attack = Mid_hash of state_hash | Replay of Vm.t | Timeout

val start :
  level:level ->
  previous_state_hash:state_hash ->
  committer:state_hash * steps ->
  rejector:state_hash * steps ->
  mid_state_hash:state_hash ->
  t

type move_result = Winner of player | Waiting of t

val defend : defend -> t -> move_result
val attack : attack -> t -> move_result

val fork : t -> t option
