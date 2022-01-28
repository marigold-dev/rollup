open Environment

type t = nat

(* O(1) *)
val current : level:level -> t

val turns_per_round : t
type turn_kind = Committer | Fork_committer | Rejector | Fork_rejector

(* O(1) *)
val turn_kind : level:level -> turn_kind
