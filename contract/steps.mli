open Environment

type t

val zero : t

val increment : t -> t

(* O(1) *)
val ( + ) : t -> t -> t

(* O(1) *)
val hash : t -> hash

(* O(1) *)
val of_nat : nat -> t option
