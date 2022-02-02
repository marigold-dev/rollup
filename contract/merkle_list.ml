open Environment

let unreachable () = failwith "unreachable"

module Make (P : sig
  type t
  val hash : t -> hash
end) =
struct
  type value =
    | Value of P.t * hash
    | Hash  of hash

  (* TODO: invariant all t must end on Hash and not be empty *)
  type t = value list

  (* HASH: blake2b("Merkle_list.t")*)
  let empty_hash = Crypto.blake2b [%bytes "4D65726B6C655F6C6973742E74"]

  let empty = [Hash empty_hash]

  let shallow hash = [Hash hash]

  let is_empty t =
    match t with
    | []
    | Hash _ :: _ :: _ ->
      unreachable ()
    | [Hash hash] -> hash = empty_hash
    | Value _ :: _ -> false

  let hash t =
    match t with
    | []
    | Hash _ :: _ :: _ ->
      unreachable ()
    | Value (_, hash) :: _
    | [Hash hash] ->
      hash

  let push n t =
    let previous_hash = hash t in
    let n_hash = Crypto.blake2b (P.hash n) in
    let hash = Crypto.blake2b (Bytes.concat n_hash previous_hash) in
    Value (n, hash) :: t

  let pop t =
    match t with
    | []
    | Hash _ :: _ :: _ ->
      unreachable ()
    | Value (n, _) :: tl -> Some (n, tl)
    | [Hash _] -> None
end
