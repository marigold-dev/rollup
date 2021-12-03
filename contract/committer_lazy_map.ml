open Environment

type t = { length : nat; items : (committer, Commit.t) big_map }

let empty () = { length = 0n; items = Big_map.empty }

let append committer commit t =
  if Big_map.mem committer t.items then None
  else
    let length = t.length + 1n in
    let items = Big_map.add committer commit t.items in
    Some { length; items }

let update committer commit t =
  (* TODO: this is unneeded but hmm *)
  if Big_map.mem committer t.items then
    let items = Big_map.add committer commit t.items in
    Some { length = t.length; items }
  else None

let remove committer t =
  (* TODO: this is unneeded but hmm *)
  if Big_map.mem committer t.items then
    let length = abs (t.length - 1n) in
    let items = Big_map.remove committer t.items in
    Some { length; items }
  else None

let find state_hash t = Big_map.find_opt state_hash t.items
let mem state_hash t = Big_map.mem state_hash t.items

let length t = t.length
