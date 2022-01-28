open Environment

type t

(* O(1) *)
val empty : unit -> t

(* O(log n) *)
val append : rejector:rejector -> Rejection_game.t -> t -> t option

(* O(log n) *)
val update : rejector:rejector -> Rejection_game.t -> t -> t option

(* O(log n) *)
val remove : rejector:rejector -> t -> t option

(* O(log n) *)
val find : rejector:rejector -> t -> Rejection_game.t option

(* O(1) *)
val length : t -> nat
