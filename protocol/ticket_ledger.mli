open Crypto

module Handle : sig
  type t = private {
    hash : BLAKE2B.t;
    id : int;
    owner : Tezos.Address.t;
    amount : Amount.t;
    ticket : Ticket.t;
  }
  [@@deriving yojson]
end

type t [@@deriving yojson]

val empty : t
val balance : Address.t -> Ticket.t -> t -> Amount.t
val transfer :
  source:Address.t ->
  destination:Address.t ->
  Amount.t ->
  Ticket.t ->
  t ->
  (t, [> `Not_enough_funds ]) result

val deposit : Address.t -> Amount.t -> Ticket.t -> t -> t
val withdraw :
  source:Address.t ->
  destination:Tezos.Address.t ->
  Amount.t ->
  Ticket.t ->
  t ->
  (t * Handle.t, [> `Not_enough_funds ]) result

val handles_find_proof : Handle.t -> t -> (BLAKE2B.t * BLAKE2B.t) list
val handles_find_proof_by_id :
  int -> t -> ((BLAKE2B.t * BLAKE2B.t) list * Handle.t) option
val handles_root_hash : t -> BLAKE2B.t
