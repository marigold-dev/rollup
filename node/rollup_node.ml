open Contract.Common

module type Side_effects = sig
  val join : unit -> unit
  val exit : unit -> unit
  val commit : Contract.commit -> unit
  val start_rejection_game : Contract.new_rejection_game -> unit
  val define_steps_for_rejection_game :
    Contract.rejection_game_id * Contract.Vm.step
end

type tezos_event =
  | Submit_event of Contract.submission
  | Commit_event of Contract.commit
  | Start_rejection_game_event of Contract.new_rejection_game
  | Define_steps_for_rejection_game of
      Contract.new_rejection_game * Contract.Vm.step
  | Send_middle_hash of Contract.rejection_game_id * state_hash

module State_machine (E : Side_effects) = struct
  type state = { current_level : int; vm_state : Contract.Vm.t }

  type transition

  let transition : state -> transition -> state = assert false
end

let listen_to_events = assert false
