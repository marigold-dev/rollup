include Ligo_environment

type player =
  | Rejector
  | Committer

type committer = address
type rejector = address

type state_hash = bytes
type input_hash = bytes
type action_hash = bytes
type level = nat

(* type committer = address
   type rejector = address

   let current_level = Tezos.level
   let sender = Tezos.sender *)

let round_time = [%nat 10]

(* TODO: ppx [%fail] to remove strings from contract *)
type submission = bytes

type operation
type _ contract
type mutez
module type Tezos = sig
  val sender : address
  val level : level
  val amount : tez

  val transaction : 'parameter -> mutez -> 'parameter contract -> operation
  val get_contract_opt : address -> 'parameter contract option
end

let stake_amount = assert false

type hash = bytes
