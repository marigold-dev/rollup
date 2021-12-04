open Environment

type t = {
  state_hash : state_hash;
  (* TODO: validate steps is inside of the commit_hash *)
  steps : steps;
  previous_rejections : Rejection_lazy_map.t option;
  rejections : Rejection_lazy_map.t;
}

let make state_hash steps =
  {
    state_hash;
    steps;
    previous_rejections = None;
    rejections = Rejection_lazy_map.empty ();
  }

let state_hash t = t.state_hash
let steps t = t.steps

let update_rejections rejections t =
  let { state_hash; steps; previous_rejections; rejections = _ } = t in
  { state_hash; steps; previous_rejections; rejections }

let append_game ~rejector rejection t =
  match Rejection_lazy_map.append ~rejector rejection t.rejections with
  | Some rejections -> Some (update_rejections rejections t)
  | None -> None

let update_game ~rejector game t =
  match Rejection_lazy_map.update ~rejector game t.rejections with
  | Some rejections -> Some (update_rejections rejections t)
  | None -> None

let start_turn t =
  let { state_hash; steps; previous_rejections = _; rejections } = t in
  let previous_rejections = Some rejections in
  Some { state_hash; steps; previous_rejections; rejections }

let fork t =
  let { state_hash; steps; previous_rejections; rejections = _ } = t in
  match previous_rejections with
  | Some rejections ->
      let commit = make state_hash steps in
      let commit = update_rejections rejections commit in
      Some commit
  | None -> None

let find_game = assert false
