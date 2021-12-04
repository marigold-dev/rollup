open Environment

type t

(* O(1) *)
val make : level:level -> state_hash -> steps:steps -> t

(* O(1) *)
val state_hash : t -> state_hash

(* O(1) *)
val steps : t -> steps

(* O(log n) *)
(* [append_game ~rejector game commit] None when duplicated *)
val append_game : rejector:rejector -> Rejection_game.t -> t -> t option

(* O(log n) *)
(* [find_game ~rejector game commit] None when missing *)
val remove_game : rejector:rejector -> t -> t option

(* O(log n) *)
(* [find_game ~rejector game commit] None when missing *)
val find_game : rejector:rejector -> t -> Rejection_game.t option

type move_result = Committer_lost | Commit of t

(* [defend ~rejector move commit] None when missing *)
val defend :
  rejector:rejector -> Rejection_game.defend -> t -> move_result option

(* [attack ~rejector move commit] None when missing *)
val attack :
  rejector:rejector -> Rejection_game.attack -> t -> move_result option

(* O(1) *)
(* [fork game] None when recently created commit *)
val fork : t -> t option
