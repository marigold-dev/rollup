type bytes

(* nat *)
type int
val int : Stdlib.Int.t -> int

(* nat *)
type nat
val nat : Stdlib.Int.t -> nat
val ( + ) : nat -> nat -> nat
val ( - ) : nat -> nat -> int
val ( * ) : nat -> nat -> nat
val ( < ) : nat -> nat -> bool
val ( >= ) : nat -> nat -> bool
val abs : int -> nat
val ediv : nat -> nat -> (nat * nat) option

module Pack : sig
  val nat : nat -> bytes
  val nat_list : nat list -> bytes

  val nat_pair_nat_bytes : nat * (nat * bytes) -> bytes

  (* server *)
  val bytes : bytes -> bytes
end

module Crypto : sig
  val blake2b : bytes -> bytes
end

module Bytes : sig
  val concat : bytes -> bytes -> bytes

  (* server *)
  val of_string : string -> bytes
end

module List : sig
  val fold_left : ('a * 'b -> 'a) -> 'a -> 'b list -> 'a
end
