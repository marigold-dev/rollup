open Environment

type t [@@deriving show { with_path = false }]

(* O(1) *)
val initial : level:nat -> t

(* O(1) *)
val execute_step : t -> t

(* O(1) *)
val halted : t -> bool

(* O(1) *)
val hash : t -> hash

(* O(1) *)
val steps : t -> steps

(* O(n) *)
val apply : nat list -> t -> t

(* O(1) *)
val single_step_data : t -> t option
