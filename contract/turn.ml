open Environment

(* A turn is a small step of a round *)
type t = nat
let current ~level =
  (* TODO: this is not needed for the smart contract *)
  let () = assert (current_level > level) in
  let levels_since_level = abs (current_level - level) in
  match levels_since_level / round_time with
  | Some (turn, _remainder) -> turn
  | None -> assert false

type turn_kind = Committer | Fork_committer | Rejector | Fork_rejector

let turns_per_round = 4n
let turn_kind ~level =
  let current_turn = current ~level in
  (* C -> F_C -> R -> F_R *)
  match current_turn / turns_per_round with
  | Some (_, remainder) ->
      if remainder = 0n then Committer
      else if remainder = 1n then Fork_committer
      else if remainder = 2n then Rejector
      else Fork_rejector
  | None -> assert false
