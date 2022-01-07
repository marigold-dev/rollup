open Environment

(* A turn is a step of a round *)
let turns_per_round = [%nat 4]
type t = nat
let current ~current_level ~level =
  (* TODO: this is not needed for the smart contract *)
  let () = assert (current_level > level) in
  let levels_since_level = abs (current_level - level) in
  match ediv levels_since_level round_time with
  | Some (turn, _remainder) -> turn
  | None -> assert false

type kind =
  | Turn_committer
  | Turn_fork_committer
  | Turn_rejector
  | Turn_fork_rejector

let kind turn =
  (* C -> F_C -> R -> F_R *)
  match ediv turn turns_per_round with
  | Some (_, remainder) ->
    if remainder = [%nat 0] then
      Turn_committer
    else if remainder = [%nat 1] then
      Turn_fork_committer
    else if remainder = [%nat 2] then
      Turn_rejector
    else
      Turn_fork_rejector
  | None -> assert false
