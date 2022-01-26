open Environment

type t = (level * committer, Commit.t) big_map

let empty () = Big_map.empty

let append level committer commit (t : t) =
  if Big_map.mem (level, committer) t then None
  else Some (Big_map.add (level, committer) commit t)

let update level committer commit (t : t) =
  (* TODO: this is unneeded but hmm *)
  (* TODO: I think get_and_update would be cheaper *)
  if Big_map.mem (level, committer) t then
    Some (Big_map.add (level, committer) commit t)
  else None

let remove level committer (t : t) =
  (* TODO: this is unneeded but hmm *)
  (* TODO: I think get_and_update would be cheaper *)
  if Big_map.mem (level, committer) t then
    Some (Big_map.remove (level, committer) t)
  else None

let find level committer (t : t) = Big_map.find_opt (level, committer) t
