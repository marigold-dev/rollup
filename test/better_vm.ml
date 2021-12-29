open Environment

type hash = bytes
module Pool : sig
  type t

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
  type t = (nat * hash) list

  let empty_hash () = Crypto.blake2b (Pack.nat_list [])
  let empty () : t = []

  let hash (t : t) =
    match t with
    | [] -> empty_hash ()
    | (_el, hash) :: _ -> hash

  let push el (t : t) : t =
    let hash_of_t = hash t in
    let hash_of_el = Crypto.blake2b (Pack.nat el) in
    let hash = Crypto.blake2b (Bytes.concat hash_of_el hash_of_t) in
    (el, hash) :: t
  let pop (t : t) =
    match t with
    | [] -> None
    | (el, _) :: tl -> Some (el, tl)

  let is_empty (t : t) =
    match t with
    | [] -> true
    | _ -> false

  let single_step_data (t : t) : t option =
    match t with
    | [] -> None
    | el :: _tl -> Some [el]
end

(* TODO: attack vector, nat is unbounded, so very large state *)
type t = {
  level : nat;
  steps : nat;
  counter : nat;
  pool : Pool.t;
}

let make ~level =
  { level; steps = [%nat 0]; counter = [%nat 0]; pool = Pool.empty () }

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
    let steps = steps + [%nat 1] in
    let counter = el + counter in
    { level; steps; counter; pool }

let hash (t : t) =
  let steps = t.steps in
  let counter = t.counter in
  let pool = Pool.hash t.pool in
  let data = (steps, (counter, pool)) in
  Crypto.blake2b (Pack.nat_pair_nat_bytes data)

let steps t = t.steps

let apply nats t =
  let { level; steps = _; counter; pool } = t in
  let level = level + [%nat 1] in
  let steps = [%nat 0] in

  let pool = List.fold_left (fun (pool, nat) -> Pool.push nat pool) pool nats in
  let pool = Pool.push [%nat 0] pool in
  let pool = Pool.push [%nat 0] pool in

  { level; steps; counter; pool }
