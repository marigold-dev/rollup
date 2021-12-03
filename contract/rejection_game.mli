open Environment

type state
type t = state

val start :
  previous_state_hash:state_hash ->
  committer:state_hash * steps ->
  rejector:state_hash * steps ->
  mid_state_hash:state_hash ->
  t

val fork : t -> t option

type turn_kind = Committer | Fork_committer | Rejector | Fork_rejector
val turn_kind : t -> turn_kind
