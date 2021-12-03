open Environment

type t

(* O(1) *)
val execute_step : t -> t

(* O(1) *)
val hash : t -> state_hash
