open Environment

module Rejector_map = Map_with_pp.Make (Address)
module Rejector_set = Set_with_pp.Make (Address)

type t = {
  good_commit : bool;
  initial_vm_state : Vm.t;
  parent_state_hash : state_hash;
  state_hash : state_hash;
  steps : steps;
  level : level;
  last_turn : Turn.t;
  previous_rejections : Rejection_game.t Rejector_map.t option;
  rejections : Rejection_game.t Rejector_map.t;
  made_rejection : bool;
  last_trusted_rejector : rejector option;
  trusted_rejectors : Rejector_set.t;
}
[@@deriving show { with_path = false }]

let current_turn t = Turn.current ~level:t.level

let parent_state_hash t = t.parent_state_hash
let state_hash t = t.state_hash
let steps t = t.steps
let level t = t.level

let make ~current_level ~level ~initial_vm_state ~good_commit ~parent_state_hash
    ~state_hash ~steps =
  let current_turn = Turn.current ~current_level ~level in
  {
    good_commit;
    initial_vm_state;
    parent_state_hash;
    state_hash;
    steps;
    level;
    last_turn = current_turn;
    previous_rejections = None;
    rejections = Rejector_map.empty;
    made_rejection = false;
    last_trusted_rejector = None;
    trusted_rejectors = Rejector_set.empty;
  }

let add_game ~rejector game t =
  let rejections = Rejector_map.add rejector game t.rejections in
  { t with rejections }
let find_game ~rejector t = Rejector_map.find rejector t.rejections
let remove_game ~rejector t =
  let rejections = Rejector_map.remove rejector t.rejections in
  { t with rejections }
let fold_games f acc t =
  Rejector_map.fold
    (fun rejector game acc -> f ~rejector game acc)
    t.rejections acc

type move_result = Rejector_won | Commit of t

let claim_winner ~rejector winner commit =
  match winner with
  | Committer ->
      let commit = remove_game ~rejector commit in
      Commit commit
  | Rejector -> Rejector_won
let handle_move_result (move_result : Rejection_game.move_result) ~rejector t =
  match move_result with
  | Winner winner -> claim_winner ~rejector winner t
  | Waiting game ->
      let t = add_game ~rejector game t in
      Commit t

let defend ~current_level ~rejector move t =
  let current_turn = current_turn ~current_level t in
  let t =
    if t.last_turn = current_turn then t
    else
      {
        t with
        last_turn = current_turn;
        previous_rejections = Some t.rejections;
      }
  in
  let game = Rejector_map.find rejector t.rejections in
  handle_move_result ~rejector (Rejection_game.defend move game) t
let attack ~current_level ~rejector move t =
  let game = Rejector_map.find rejector t.rejections in

  let is_relevant =
    (not t.good_commit) && (not t.made_rejection)
    && Rejector_set.mem rejector t.trusted_rejectors
  in
  let trusted_rejectors =
    if is_relevant then
      let expected_move =
        Rejection_game.find_attack ~initial_vm_state:t.initial_vm_state game
      in
      if expected_move = move then t.trusted_rejectors
      else Rejector_set.remove rejector t.trusted_rejectors
    else t.trusted_rejectors
  in

  let t = { t with trusted_rejectors } in
  handle_move_result ~rejector
    (Rejection_game.attack ~current_level move game)
    t

let fork ~current_level t =
  let {
    good_commit;
    initial_vm_state;
    parent_state_hash;
    state_hash;
    steps;
    level;
    last_turn = _;
    previous_rejections;
    rejections = _;
    made_rejection;
    last_trusted_rejector;
    trusted_rejectors;
  } =
    t
  in
  match previous_rejections with
  | Some rejections ->
      let t =
        make ~current_level ~level ~initial_vm_state ~good_commit
          ~parent_state_hash ~state_hash ~steps
      in
      {
        t with
        made_rejection;
        trusted_rejectors;
        last_trusted_rejector;
        rejections;
      }
  | None -> assert false

let good_commit t = t.good_commit
let self_made_rejection t = t.made_rejection

let find_defend ~rejector t =
  let game = Rejector_map.find rejector t.rejections in
  Rejection_game.find_defend ~initial_vm_state:t.initial_vm_state game
let find_defends t =
  fold_games
    (fun ~rejector game moves ->
      ( rejector,
        Rejection_game.find_defend ~initial_vm_state:t.initial_vm_state game )
      :: moves)
    [] t

let needs_rejector t =
  (not t.good_commit) && (not t.made_rejection)
  && Rejector_set.is_empty t.trusted_rejectors
let made_rejection t = { t with made_rejection = true }
let last_trusted_rejector t = t.last_trusted_rejector
