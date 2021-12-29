include Z_int
include Z_nat

type bytes = string
(*
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

     val get_and_update :
       'key ->
       'value option ->
       ('key, 'value) big_map ->
       'value option * ('key, 'value) big_map
   end = struct
     let empty = assert false

     let find_opt = assert false

     let add = assert false

     let remove = assert false

     let mem = assert false

     let get_and_update = assert false
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
   end *)

module Pack = struct
  open Tezos.Pack
  let to_string p = to_bytes p |> Bytes.to_string
  let bytes s = bytes (Bytes.of_string s)
  let nat n = nat (Z_nat.Read.to_z n)
  let nat_list l = list (List.map nat l)
  let nat_pair_nat_bytes (n1, (n2, b)) = pair (nat n1) (pair (nat n2) (bytes b))

  let bytes b = to_string (bytes b)
  let nat n = to_string (nat n)
  let nat_list n = to_string (nat_list n)
  let nat_pair_nat_bytes x = to_string (nat_pair_nat_bytes x)
end

module Crypto = struct
  open Crypto
  let blake2b t = BLAKE2B.to_raw_string (BLAKE2B.hash t)
end

module List = struct
  let fold_left f acc l = Stdlib.List.fold_left (fun a b -> f (a, b)) acc l
end
module Bytes = struct
  let concat = Stdlib.( ^ )
  let of_string x = x
end
