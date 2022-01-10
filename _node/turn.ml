open Environment

(* A turn is a small step of a round *)
type t = nat
let round_time = Z.of_int 10

let current ~current_level ~level =
  (* TODO: this is not needed for the smart contract *)
  let () = assert (current_level >= level) in
  let levels_since_level = abs (current_level - level) in
  levels_since_level / round_time

type turn_kind =
  | Committer
  | Fork_committer
  | Rejector
  | Fork_rejector

let turns_per_round = Z.of_int 4
let turn_kind ~current_level ~level =
  let current_turn = current ~current_level ~level in
  let remainder = current_turn mod turns_per_round in
  if remainder = Z.zero then
    Committer
  else if remainder = Z.one then
    Fork_committer
  else if remainder = Z.of_int 2 then
    Rejector
  else
    Fork_rejector
