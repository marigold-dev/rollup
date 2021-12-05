open Environment

type t

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
