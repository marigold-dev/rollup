open Environment

type t

(* O(1) *)
val execute_step : t -> t

(* O(1) *)
val halted : t -> bool

(* O(1) *)
val hash : t -> hash

(* O(1) *)
val steps : t -> Steps.t

(* O(1) *)
(* TODO: explain better *)
(* used to calculate the hash without having the actual data *)
(* TODO: does this function makes sense here? *)
val make_initial_hash :
  level:level ->
  previous_state_hash:state_hash ->
  initial_input_hash:input_hash ->
  state_hash

(* O(1) *)
(* TODO: explain better *)
(* used to calculate the hash after computing everything,
   input hash will be the empty hash *)
(* TODO: does this function makes sense here? *)
val make_final_hash :
  level:level -> final_state_hash:state_hash -> final_step:Steps.t -> state_hash
