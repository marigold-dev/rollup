open Environment

type t

(* O(1) *)
val shallow : t -> t

(* O(1) *)
val execute_step : t -> t

(* O(1) *)
val halted : t -> bool

(* O(1) *)
val hash : t -> hash

(* TODO: should those 3 functions be here? *)
(* O(1) *)
val initial :
  level:level ->
  previous_storage_hash:storage_hash ->
  initial_input_hash:input_hash ->
  state_hash

(* O(1) *)
val make_mid_state_hash :
  level:level -> step:Step.t -> input_storage_hash:hash -> state_hash

(* O(1) *)
val make_final_state_hash :
  level:level ->
  final_step:Step.t ->
  final_storage_hash:state_hash ->
  state_hash
