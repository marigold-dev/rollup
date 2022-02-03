(*
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

  let push (el : nat) (t : t) : t =
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

module Int32 : sig
  type t

  (* O(1) *)
  val zero : t

  (* O(1) *)
  val one : t

  (* O(1) *)
  val ( + ) : t -> t -> t

  (* O(1) *)
  val ( - ) : t -> t -> t

  (* O(1) *)
  val hash : t -> hash
end = struct
  type t
  let zero = assert false
  let one = assert false
  let ( + ) = assert false
  let ( - ) = assert false
  let hash = assert false
end

type instruction =
  (* X : Y : ... -> Z : ... subtract (X - Y = Z) *)
  | SUB
  (* X : ... -> X : X : ... duplicate X *)
  | DUP
  (* A -> INT32 : ... insert a constant int *)
  | CONST of Int32.t
  (* X : ADDRESS : ... -> ... jump if zero to absolute address *)
  | JMPZ
  (* A -> A. halts *)
  | HLT

module Program : sig
  type t
  val read : Int32.t -> t -> instruction option
end = struct
  type t
  let read = assert false
end
module Stack : sig
  type t

  (* O(1) *)
  val empty : unit -> t

  (* O(1) *)
  val hash : t -> hash

  (* O(1) *)
  val push : Int32.t -> t -> t

  (* O(1) *)
  val pop : t -> (Int32.t * t) option
end = struct
  (* TODO: that doesn't work to provide partial data verifiable on Tezos *)
  type t = (Int32.t * hash) list

  let empty () = []
  let empty_hash = Crypto.blake2b [%bytes ""]

  let hash (t : t) =
    match t with
    
    | (_el, hash) :: _tl -> hash
    | [] -> empty_hash

  let push el (t : t) : t =
    let hash_of_t = hash t in
    let hash_of_el = Int32.hash el in
    let hash = Crypto.blake2b (Bytes.concat hash_of_el hash_of_t) in
    (el, hash) :: t

  let pop (t : t) =
    match t with
    | (el, _hash) :: tl -> Some (el, tl)
    | [] -> None
end

type state = {
  program : Program.t;
  program_counter : Int32.t;
  (* cache to program[program_counter] *)
  next_instruction : instruction;
  stack : Stack.t;
  steps : nat;
  halted : bool;
}

let bind v f =
  match v with
  | Some v -> f v
  | None -> None
let ( let* ) = bind

open Int32

(* TODO: execute return option for many reasons, fix that *)
let step state =
  let { program; program_counter; next_instruction; stack; steps; halted = _ } = state in
  let jump stack program_counter =
    match Program.read program_counter program with
    | Some next_instruction ->
      Some { program; program_counter; next_instruction; stack }
    | None -> None in
  let next stack =
    let program_counter = program_counter + one in
    jump stack program_counter in

  match next_instruction with
  | SUB ->
    let* x, stack = Stack.pop stack in
    let* y, stack = Stack.pop stack in
    let z = x - y in
    let stack = Stack.push z stack in
    next stack
  | DUP ->
    let* x, stack = Stack.pop stack in
    let stack = Stack.push x stack in
    let stack = Stack.push x stack in
    next stack
  | CONST x ->
    let stack = Stack.push x stack in
    next stack
  | JMPZ ->
    let* x, stack = Stack.pop stack in
    let* program_counter, stack = Stack.pop stack in
    if x = zero then
      jump stack program_counter
    else
      next stack
  | HLT -> ()
(* TODO: discuss if this failwith makes sense *)
let step state = match ste
(* TODO: attack vector, nat is unbounded, so very large state *)
type t = {
  level : nat;
  steps : nat;
  counter : nat;
  pool : Pool.t;
}

let _empty = Pool.empty
let _single_step_data t =
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
*)

open Environment

let unreachable () = failwith "unreachable"

type t = {
  input : Nat32_merkle_list.t;
  (* TODO: this level is only used to ensure uniqueness of hashes
           and also to have a single step at least *)
  level : level;
  steps : Steps.t;
  state : Nat32.t;
}

let execute_step t =
  let { input; level; steps; state } = t in
  let steps = Steps.increment steps in

  if Steps.zero = steps then
    let level = level + [%nat 1] in
    { input; level; steps; state }
  else
    let n, input =
      match Nat32_merkle_list.pop input with
      | Some (n, input) -> (n, input)
      | None -> unreachable () in
    let state = Nat32.(n + state) in
    { input; level; steps; state }

let halted t = t.steps <> Steps.zero && Nat32_merkle_list.is_empty t.input

let compute_hash ~input_hash ~level_hash ~steps_hash ~state_hash =
  Crypto.blake2b
    (Bytes.concat
       (Bytes.concat input_hash level_hash)
       (Bytes.concat steps_hash state_hash))
let hash_nat nat = Crypto.blake2b (Pack.nat nat)
let hash t =
  let { input; level; steps; state } = t in
  let input_hash = Nat32_merkle_list.hash input in
  let level_hash = hash_nat level in
  let steps_hash = Steps.hash steps in
  let state_hash = Nat32.hash state in
  compute_hash ~input_hash ~level_hash ~steps_hash ~state_hash

let steps t = t.steps

let make_initial_hash ~level ~previous_state_hash ~initial_input_hash =
  (* TODO: level cannot be zero *)
  let previous_level = abs (level - [%nat 1]) in
  let level_hash = hash_nat previous_level in
  let steps_hash = Steps.hash Steps.zero in
  compute_hash ~input_hash:initial_input_hash ~level_hash ~steps_hash
    ~state_hash:previous_state_hash
let make_final_hash ~level ~final_state_hash ~final_step =
  let level_hash = hash_nat level in
  let input_hash = Nat32_merkle_list.(hash empty) in
  let final_steps_hash = Steps.hash final_step in
  compute_hash ~input_hash ~level_hash ~steps_hash:final_steps_hash
    ~state_hash:final_state_hash
