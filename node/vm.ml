open Environment

module Crypto = struct
  let blake2b bytes =
    Stdlib.Bytes.to_string bytes
    |> Crypto.BLAKE2B.hash |> Crypto.BLAKE2B.to_raw_string
    |> Stdlib.Bytes.of_string
end
module Pack = struct
  open Tezos.Pack
  let empty_list = list [] |> to_bytes
  let data (level, (steps, (counter, pool))) =
    pair (nat level) (pair (nat steps) (pair (nat counter) (bytes pool)))
    |> to_bytes
  let nat n = nat n |> to_bytes
end
type hash = bytes

module Pool : sig
  type t [@@deriving show { with_path = false }]

  (* O(1) *)
  val empty : unit -> t

  (* O(1) *)
  val hash : t -> hash

  (* O(1) *)
  val push : nat -> t -> t

  (* O(1) *)
  val pop : t -> (nat * t) option

  (* O(1) *)
  val is_empty : t -> bool

  (* O(1) *)
  val single_step_data : t -> t option
end = struct
  type t = (nat * hash) list [@@deriving show { with_path = false }]

  let empty_hash () = Crypto.blake2b Pack.empty_list
  let empty () = []

  let hash (t : t) =
    match t with [] -> empty_hash () | (_el, hash) :: _ -> hash

  let push (el : nat) (t : t) : t =
    let hash_of_t = hash t in
    let hash_of_el = Crypto.blake2b (Pack.nat el) in
    let hash = Crypto.blake2b (Bytes.concat hash_of_el hash_of_t) in
    (el, hash) :: t
  let pop (t : t) = match t with [] -> None | (el, _) :: tl -> Some (el, tl)

  let is_empty (t : t) = match t with [] -> true | _ -> false

  let single_step_data (t : t) : t option =
    match t with [] -> None | el :: _tl -> Some [ el ]
end

let _ = Pool.show

(* TODO: attack vector, nat is unbounded, so very large state *)
type t = { level : nat; steps : nat; counter : nat; pool : Pool.t }
[@@deriving show { with_path = false }]

let initial ~level =
  { level; steps = Z.zero; counter = Z.zero; pool = Pool.empty () }
let single_step_data t =
  let { level; steps; counter; pool } = t in
  match Pool.single_step_data pool with
  | Some pool -> Some { level; steps; counter; pool }
  | None -> None

let halted t = Pool.is_empty t.pool
let execute_step t =
  let { level; steps; counter; pool } = t in
  match Pool.pop pool with
  | None ->
      (* can only happen if not enough data was sent *)
      assert false
  | Some (el, pool) ->
      let steps = steps + Z.one in
      let counter = el + counter in
      { level; steps; counter; pool }

let hash (t : t) =
  let level = t.level in
  let steps = t.steps in
  let counter = t.counter in
  let pool = Pool.hash t.pool in
  let data = (level, (steps, (counter, pool))) in
  Crypto.blake2b (Pack.data data)

let steps t = t.steps

let apply nats t =
  let { level; steps = _; counter; pool } = t in
  let level = level + Z.one in
  let steps = Z.zero in

  let pool = List.fold_left (fun pool nat -> Pool.push nat pool) pool nats in
  let pool = Pool.push Z.zero pool in
  let pool = Pool.push Z.zero pool in
  { level; steps; counter; pool }
