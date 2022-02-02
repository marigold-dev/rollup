open Environment

type t

val zero : t

val one : t

(* O(1) *)
val increment : t -> t

(* O(1) *)
val mid_step : initial_step:t -> final_step:t -> t

(* O(1) *)
val ( + ) : t -> t -> t

(* O(1) *)
val min : t -> t -> t

(* O(1) *)
val hash : t -> hash

(* O(1) *)
val of_nat : nat -> t option

(* TODO: final step can never be smaller than 2 *)

type non_zero

val is_non_zero : t -> non_zero option

val of_non_zero : non_zero -> t
