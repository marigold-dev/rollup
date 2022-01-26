open Environment

type commit
type cell = {
  commit : commit;
  (* TODO: we could also use a double linked list, is that worth it? *)
  removed : bool;
  next : committer option;
}

type t = {
  values : (level * committer, cell) big_map;
  (* TODO: this should be on level_data *)
  lasts : (level, committer) big_map;
  pending : level option;
}

let append level committer commit t =
  if Big_map.mem (level, committer) t.values then None
  else
    let cell =
      let last = Big_map.find_opt level t.lasts in
      { commit; removed = false; next = last }
    in
    let values = Big_map.add (level, committer) cell t.values in
    let lasts = Big_map.add level committer t.lasts in
    Some { values; lasts }

let remove_head level t =
  let { values; lasts } = t in
  match Big_map.get_and_update level None lasts with
  | Some head_committer, lasts ->
      let next, values =
        match Big_map.get_and_update (level, head_committer) None values with
        | Some head, tail -> (head.next, tail)
        | None, _tail -> failwith "unreachable"
      in

      let lasts =
        match next with
        | Some next -> Big_map.add level next lasts
        | None -> lasts
      in

      Some { values; lasts }
  | None, _lasts -> None
