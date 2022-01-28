open Environment

type t

(* O(1) *)
val empty : unit -> t

(* O(log n) *)
(* [append level committer commit t] None when duplicated *)
val append : level -> committer -> Commit.t -> t -> t option

(* O(log n) *)
(* [update commilevel tter commit t] None when missing *)
val update : level -> committer -> Commit.t -> t -> t option

(* O(log n) *)
(* [remove level committer t] None when missing *)
val remove : level -> committer -> t -> t option

(* O(log n) *)
(* [find level committer t] None when missing *)
val find : level -> committer -> t -> Commit.t option
