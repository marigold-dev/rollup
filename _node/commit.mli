open Environment

type t [@@deriving show { with_path = false }]

val parent_state_hash : t -> state_hash
val state_hash : t -> state_hash
val steps : t -> steps
val level : t -> level

val make :
  current_level:level ->
  level:level ->
  initial_vm_state:Vm.t ->
  good_commit:bool ->
  parent_state_hash:state_hash ->
  state_hash:state_hash ->
  steps:steps ->
  t
val add_game : rejector:rejector -> Rejection_game.t -> t -> t
val find_game : rejector:rejector -> t -> Rejection_game.t

val fork : current_level:level -> t -> t

type move_result = Rejector_won | Commit of t
val defend :
  current_level:level ->
  rejector:rejector ->
  Rejection_game.defend ->
  t ->
  move_result
val attack :
  current_level:level ->
  rejector:rejector ->
  Rejection_game.attack ->
  t ->
  move_result

val find_defend : rejector:rejector -> t -> Rejection_game.defend
val find_defends : t -> (rejector * Rejection_game.defend) list

val good_commit : t -> bool
val self_made_rejection : t -> bool
val needs_rejector : t -> bool
val made_rejection : t -> t
val last_trusted_rejector : t -> rejector option
