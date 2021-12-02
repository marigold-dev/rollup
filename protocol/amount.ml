type t = int [@@deriving ord]
let zero = 0

let ( + ) = ( + )
let ( - ) a b =
  let t = a - b in
  assert (t >= zero);
  t
let of_int t =
  (* TODO: test this, should amount be non-zero? *)
  assert (t >= zero);
  t

let to_int t = t

let of_yojson json = json |> [%of_yojson: int] |> Result.map of_int
let to_yojson t = `Int t
