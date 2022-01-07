open Environment

type t

(* O(1) *)
val current : current_level:level -> level:level -> t

type kind = private
  | Turn_committer
  | Turn_fork_committer
  | Turn_rejector
  | Turn_fork_rejector

(* O(1) *)
val kind : t -> kind
