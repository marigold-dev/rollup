open Environment

type state
type t = state [@@deriving show { with_path = false }]
type vote = Agree | Disagree [@@deriving show { with_path = false }]
type defend = Vote of vote | Timeout [@@deriving show { with_path = false }]

type attack = Mid_hash of state_hash | Replay of Vm.t | Timeout
[@@deriving show { with_path = false }]

val start :
  current_level:level ->
  level:level ->
  previous_state_hash:state_hash ->
  committer:state_hash * steps ->
  rejector:state_hash * steps ->
  mid_state_hash:state_hash ->
  t

type move_result = Winner of player | Waiting of t

val defend : defend -> t -> move_result
val attack : current_level:level -> attack -> t -> move_result

val find_defend : initial_vm_state:Vm.t -> t -> defend
val find_mid_hash :
  committer_steps:steps ->
  rejector_steps:steps ->
  initial_vm_state:Vm.t ->
  state_hash
val find_attack : initial_vm_state:Vm.t -> t -> attack

val fork : current_level:level -> t -> t
