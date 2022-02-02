open Environment

module Make (P : sig
  type t
  val hash : t -> hash
end) : sig
  type t

  (* O(1) *)
  val empty : t

  (* O(1) *)
  val is_empty : t -> bool

  (* O(1) *)
  (* TODO: this function is used to create a list
           where the remaining elements are missing *)
  (* [shallow base_hash] creates a shallow Merkle_list *)
  val shallow : hash -> t

  (* O(1) *)
  val hash : t -> hash

  (* O(1) *)
  val push : P.t -> t -> t

  (* O(1) *)
  val pop : t -> (P.t * t) option
end
