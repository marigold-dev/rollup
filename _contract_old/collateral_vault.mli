open Environment

type t

(* O(log2 length) *)
val has_stake : address -> t -> bool

(* O(log2 length) *)
val join : address -> t -> t

(* O(log2 length) *)
val burn : address -> t -> t
