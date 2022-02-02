open Environment

type t

val zero : t

val one : t

(* O(1) *)
val ( + ) : t -> t -> t

(* O(1) *)
val diff : t -> t -> t

(* O(1) *)
val ediv : t -> t -> (t * t) option

(* O(1) *)
val min : t -> t -> t

(* O(1) *)
val hash : t -> hash

(* O(1) *)
val of_nat : nat -> t option

(* O(1) *)
val of_stdlib_int_exn : Stdlib.Int.t -> t
