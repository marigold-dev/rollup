open Environment

type t

(* O(1) *)

val make : level:level -> t

(* O(1) *)
val single_step_data : t -> t option

(* O(1) *)
val execute_step : t -> t

(* O(1) *)
val halted : t -> bool

(* O(1) *)
val hash : t -> state_hash

(* O(1) *)
val steps : t -> steps

(* O(n) *)
val apply : nat list -> t -> t
