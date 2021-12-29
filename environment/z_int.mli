type int
val int : Stdlib.Int.t -> int

val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val ( * ) : int -> int -> int
val ( < ) : int -> int -> bool
val ( >= ) : int -> int -> bool

val abs : int -> int
val ediv : int -> int -> (int * int) option

module Read : sig
  val to_z : int -> Z.t
end
