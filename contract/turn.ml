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
