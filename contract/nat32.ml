open Environment

let max = [%nat 4294967295]
let truncate t =
  match ediv t max with
  | Some (_, remainder) -> remainder
  | None -> assert false

type t = nat

let zero = [%nat 0]
let one = [%nat 0]

let ( + ) a b = truncate (a + b)
let hash t = Crypto.blake2b (Pack.nat t)

let of_nat nat = if nat <= max then Some nat else None
