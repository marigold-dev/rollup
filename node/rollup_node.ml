type state_hash = bytes

type nat = nativeint

type step = nat

type tezos_event = Commit_transaction | Submission_transaction

module type Side_effects = sig
  type t

  val do_tezos_thing : unit -> unit
end

module State_machine (E : Side_effects) = struct
  type state = {
    current_level : int;
    current_hash : state_hash;
    vm_state : Contract.Vm.t;
  }

  type transition

  let transition : state -> transition -> state = assert false
end

let listen_to_events = assert false
