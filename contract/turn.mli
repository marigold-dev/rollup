open Environment

type t = nat

(* O(1) *)
val current : level:t -> t
