type t = Nat32.t

let zero = Nat32.zero
let one = Nat32.one
let two = Nat32.(one + one)

let increment n = Nat32.(n + one)

let mid_step ~initial_step ~final_step =
  let diff = Nat32.diff final_step initial_step in
  match Nat32.ediv diff two with
  | Some (step_offset, _remainder) -> Nat32.(initial_step + step_offset)
  | None -> assert false

let ( + ) = Nat32.( + )
let min = Nat32.min

let hash = Nat32.hash
let of_nat = Nat32.of_nat

type non_zero = t
let is_non_zero t = if t <> zero then Some t else None
let of_non_zero t = t
