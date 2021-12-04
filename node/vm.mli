open Common
type t = int

val run_submissions : ?until_step:int -> submission list -> t -> t * step_count
val hash_state : t -> Common.hash
