open Environment

type t = {
  (* constants *)
  state_hash : state_hash;
  (* TODO: validate steps is inside of the commit_hash *)
  steps : steps;
  (* duplicated *)
  level : level;
  last_turn : Turn.t;
  (* state *)
  previous_rejections : Rejection_lazy_map.t option;
  rejections : Rejection_lazy_map.t;
}

let current_turn t = Turn.current ~level:t.level
let turn_kind t = Turn.turn_kind ~level:t.level

let make ~level state_hash ~steps =
  let current_turn = Turn.current ~level in
  {
    state_hash;
    steps;
    level;
    last_turn = current_turn;
    previous_rejections = None;
    rejections = Rejection_lazy_map.empty ();
  }

let state_hash t = t.state_hash
let steps t = t.steps

let append_game ~rejector game t =
  match Rejection_lazy_map.append ~rejector game t.rejections with
  | Some rejections -> Some { t with rejections }
  | None -> None

let remove_game ~rejector t =
  match Rejection_lazy_map.remove ~rejector t.rejections with
  | Some rejections -> Some { t with rejections }
  | None -> None

let find_game ~rejector t = Rejection_lazy_map.find ~rejector t.rejections

let update_game ~rejector game t =
  match Rejection_lazy_map.update ~rejector game t.rejections with
  | Some rejections -> Some { t with rejections }
  | None -> None

type move_result = Committer_lost | Commit of t

let claim_winner ~rejector winner commit =
  match winner with
  | Committer -> (
      match remove_game ~rejector commit with
      | Some commit -> Some (Commit commit)
      | None -> None)
  | Rejector -> Some Committer_lost
let handle_move_result (move_result : Rejection_game.move_result) ~rejector t =
  match move_result with
  | Winner winner -> claim_winner ~rejector winner t
  | Waiting game -> (
      match update_game ~rejector game t with
      | Some t -> Some (Commit t)
      | None -> None)

(*
  last_turn != current_turn;
  A;                         D; A*)
(*
   0 -> 1   -> 2 -> 3 -> 4
   C -> F_C -> R -> W -> R
*)
let defend ~rejector move t =
  let current_turn = current_turn t in
  let t =
    if t.last_turn = current_turn then t
    else
      let () = assert (turn_kind t = Committer) in
      {
        t with
        last_turn = current_turn;
        previous_rejections = Some t.rejections;
      }
  in

  match Rejection_lazy_map.find ~rejector t.rejections with
  | Some game ->
      handle_move_result ~rejector (Rejection_game.defend move game) t
  | None -> None

let attack ~rejector move t =
  match Rejection_lazy_map.find ~rejector t.rejections with
  | Some game ->
      handle_move_result ~rejector (Rejection_game.attack move game) t
  | None -> None

let fork t =
  let {
    state_hash;
    steps;
    level;
    last_turn = _;
    previous_rejections;
    rejections = _;
  } =
    t
  in
  (* TODO: prevent duplicating forking early if it matters *)
  let () = assert (Turn.turn_kind ~level = Fork_committer) in

  match previous_rejections with
  | Some rejections ->
      let t = make ~level state_hash ~steps in
      Some { t with rejections }
  | None -> None
