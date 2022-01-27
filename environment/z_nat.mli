open Z_int

type nat
val nat : Stdlib.Int.t -> nat

val ( + ) : nat -> nat -> nat
val ( - ) : nat -> nat -> int
val ( * ) : nat -> nat -> nat
val ( < ) : nat -> nat -> bool
val ( >= ) : nat -> nat -> bool

val abs : int -> nat
val ediv : nat -> nat -> (nat * nat) option
val min : nat -> nat -> nat

module Read : sig
  val to_z : nat -> Z.t
end
