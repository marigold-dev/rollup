type address
type (_, _) big_map

(* bytes *)
type bytes
val bytes : Stdlib.String.t -> bytes

(* int *)
type int
val int : Stdlib.Int.t -> int

(* tez *)
type tez
val tez : Stdlib.Int.t -> tez

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
val min : nat -> nat -> nat

module Pack : sig
  val nat : nat -> bytes
  val nat_list : nat list -> bytes

  val nat_pair_nat_bytes : nat * (nat * bytes) -> bytes
end

(* TODO: maybe higher level APIs fusing pack and blake2b? *)
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

module Big_map : sig
  val empty : ('key, 'value) big_map

  val find_opt : 'key -> ('key, 'value) big_map -> 'value option

  val add : 'key -> 'value -> ('key, 'value) big_map -> ('key, 'value) big_map

  val remove : 'key -> ('key, 'value) big_map -> ('key, 'value) big_map

  val mem : 'key -> ('key, 'value) big_map -> bool
end
