open Environment

type t = { length : nat; items : (rejector, Rejection_game.t) big_map }

let empty () = { length = 0n; items = Big_map.empty }

let append ~rejector game t =
  if Big_map.mem rejector t.items then None
  else
    let length = t.length + 1n in
    let items = Big_map.add rejector game t.items in
    Some { length; items }

let update ~rejector game t =
  if Big_map.mem rejector t.items then
    let items = Big_map.add rejector game t.items in
    Some { length = t.length; items }
  else None

let remove ~rejector t =
  (* TODO: assert properties required to remove a rejection game *)
  if Big_map.mem rejector t.items then
    let length = abs (t.length - 1n) in
    let items = Big_map.remove rejector t.items in
    Some { length; items }
  else None

let find ~rejector t = Big_map.find_opt rejector t.items

let length t = t.length
