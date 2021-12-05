type nat = nativeint

type (_, _) big_map

type (_, _) map

type _ set

type mutez = nat

type tez = nat

type operation

type address

type _ contract

let ( + ) : nat -> nat -> nat = assert false

let ( - ) : nat -> nat -> int = assert false

let ( * ) : nat -> nat -> nat = assert false

let ( < ) : nat -> nat -> bool = assert false

let ( >= ) : nat -> nat -> bool = assert false

let min : nat -> nat -> nat = assert false

let ( / ) : nat -> nat -> (nat * nat) option = assert false

let abs : int -> nat = assert false

module Tezos = struct
  let level : nat = assert false

  let amount : tez = assert false

  let sender : address = assert false

  let transaction : 'parameter -> mutez -> 'parameter contract -> operation =
    assert false

  let get_contract_opt : address -> 'parameter contract option = assert false
end

module Big_map : sig
  val empty : ('key, 'value) big_map

  val find_opt : 'key -> ('key, 'value) big_map -> 'value option

  val add : 'key -> 'value -> ('key, 'value) big_map -> ('key, 'value) big_map

  val remove : 'key -> ('key, 'value) big_map -> ('key, 'value) big_map

  val mem : 'key -> ('key, 'value) big_map -> bool
end = struct
  let empty = assert false

  let find_opt = assert false

  let add = assert false

  let remove = assert false

  let mem = assert false
end

module Set : sig
  val add : 'el -> 'el set -> 'el set

  val empty : 'a set

  val cardinal : 'a set -> nat

  val fold : ('acc * 'el -> 'acc) -> 'el set -> 'acc -> 'acc
end = struct
  let add = assert false

  let empty = assert false

  let cardinal = assert false

  let fold = assert false
end

module Bytes : sig
  type t = bytes
  val pack : 'a -> t
  val concat : bytes -> bytes -> bytes
end = struct
  type t = bytes
  let pack = assert false
  let concat = assert false
end

module Crypto : sig
  val blake2b : bytes -> bytes
end = struct
  let blake2b = assert false
end
