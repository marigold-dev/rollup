open Environment

type t

(* O(log n) *)
val append : level -> committer -> Commit.t -> t -> t option

(* O(log n) *)
val find : level -> committer -> t -> Commit.t option

(* O(log n)*)
val remove : level -> committer -> t -> t option

(* O(log n) *)
val length : level -> t -> nat

(* O(log n) *)
val free_step : t -> t
