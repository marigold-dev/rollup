open Environment

(* A turn is a step of a round *)
let turns_per_round = [%nat 4]
let ediv_turns_per_round t =
  match ediv t turns_per_round with
  | Some result -> result
  | None -> assert false

type t = nat
let first_committer_turn = [%nat 0]
let first_rejector_turn = [%nat 2]

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
  let _, remainder = ediv_turns_per_round turn in
  if remainder = [%nat 0] then
    Turn_committer
  else if remainder = [%nat 1] then
    Turn_fork_committer
  else if remainder = [%nat 2] then
    Turn_rejector
  else
    Turn_fork_rejector

(* TODO: validate that when we move to both players at same time *)
let must_timeout ~last_opposite_player_turn ~current_turn =
  abs (current_turn - last_opposite_player_turn) >= round_time

let should_undo_move ~last_player_turn ~current_turn =
  last_player_turn + [%nat 1] = current_turn
