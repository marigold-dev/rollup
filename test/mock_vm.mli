open Environment

type t

(* O(1) *)

val make : hashes:bytes list -> t

(* O(1) *)
val single_step_data : t -> t option

(* O(1) *)
val execute_step : t -> t

(* hash *)
val halted : bytes

(* O(1) *)
val hash : t -> state_hash
