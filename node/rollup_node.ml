type state_hash = bytes
type nat = nativeint
type step = nat

type tezos_event = 
| Commit_transaction
| Submission_transaction

module type Side_effects = sig
  type t
  val  do_tezos_thing : unit -> unit
end

module VM : sig
  type action
  type t

  val execute_step : t -> t

  val apply : action -> t -> t

  val hash : t -> state_hash
  val step : t -> step
end = struct
  type action
  type t

  let execute_step _ = assert false

  let apply _ _ = assert false

  let hash _ = assert false
  let step _ = assert false
end

module State_machine (E: Side_effects) = struct
  type state = {
    current_level: int;
    current_hash : state_hash;
    vm_state : VM.t
  }

  type transition
  let transition :  state -> transition -> state = assert false
end

let listen_to_events = assert false
