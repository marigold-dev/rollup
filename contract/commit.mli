open Environment

type t

(* O(1) *)
val make : state_hash -> steps -> t

(* O(1) *)
val state_hash : t -> state_hash

(* O(1) *)
val steps : t -> steps

(* O(log n) *)
(* [append_game ~rejector game commit] None when duplicated *)
val append_game : rejector:rejector -> Rejection_game.t -> t -> t option

(* O(log n) *)
(* [update_game ~rejector game commit] None when missing *)
val update_game : rejector:rejector -> Rejection_game.t -> t -> t option

(* O(log n) *)
(* [find_game ~rejector game commit] None when missing *)
val find_game : rejector:rejector -> t -> t option

(* O(1) *)
(* [fork game] None when recently created commit *)
val fork : t -> t option
