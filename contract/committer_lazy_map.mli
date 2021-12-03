open Environment

type t

val empty : unit -> t

(* O(log n) *)
(* [append state_hash] None when duplicated *)
val append : committer -> Commit.t -> t -> t option

(* O(log n) *)
(* [update committer commit t] None when missing *)
val update : committer -> Commit.t -> t -> t option

(* O(log n) *)
(* [remove committer t] None when missing *)
val remove : committer -> t -> t option

(* O(log n) *)
val find : committer -> t -> Commit.t option

(* O(log n) *)
val mem : committer -> t -> bool

(* O(1) *)
(* TODO: why do we need the length???*)
val length : t -> nat
