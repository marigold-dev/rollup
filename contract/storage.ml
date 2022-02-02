open Environment

module Level_data : sig
  type t

  (* O(1) *)
  val empty : unit -> t

  (* O(1) *)
  val input_hash : t -> input_hash

  (* O(1) *)
  val commits : t -> nat

  (* O(1) *)
  val action_added : action_hash -> t -> t

  (* O(1) *)
  val commit_added : t -> t

  (* O(1) *)
  val commit_removed : t -> t
end = struct
  (* HASH: blake2b("Level_data.input_hash") *)
  let empty_input_hash =
    let seed = [%bytes "4C6576656C5F646174612E696E7075745F68617368"] in
    Crypto.blake2b seed

  type t = {
    input_hash : input_hash;
    commits : nat;
  }

  let empty () = { input_hash = empty_input_hash; commits = [%nat 0] }

  let input_hash t = t.input_hash
  let commits t = t.commits

  let action_added action_hash t =
    let { input_hash; commits } = t in
    let input_hash = Crypto.blake2b (Bytes.concat action_hash input_hash) in
    { input_hash; commits }

  let commit_added t =
    let { input_hash; commits } = t in
    let commits = commits + [%nat 1] in
    { input_hash; commits }
  let commit_removed t =
    let { input_hash; commits } = t in
    let commits = abs (commits - [%nat 1]) in
    { input_hash; commits }
end

module Commit_data : sig
  type t

  (* O(1) *)
  val make :
    previous_state_hash:state_hash -> state_hash:state_hash -> steps:steps -> t

  (* O(1) *)
  val previous_state_hash : t -> state_hash

  (* O(1) *)
  val state_hash : t -> state_hash

  (* O(1) *)
  val steps : t -> steps

  (* O(1) *)
  val games : t -> nat

  (* O(1) *)
  val game_added : t -> t

  (* O(1) *)
  val game_removed : t -> t
end = struct
  type t = {
    previous_state_hash : state_hash;
    state_hash : state_hash;
    (* TODO: validate steps is inside of the state_hash *)
    steps : steps;
    games : nat;
  }

  let make ~previous_state_hash ~state_hash ~steps =
    { previous_state_hash; state_hash; steps; games = [%nat 0] }

  let previous_state_hash t = t.previous_state_hash
  let state_hash t = t.state_hash
  let steps t = t.steps
  let games t = t.games

  let game_added t = { t with games = t.games + [%nat 1] }
  let game_removed t = { t with games = abs (t.games - [%nat 1]) }
end

module Garbage : sig
  (* collected functions should only be called when the thing itself was in
     the garbage, not everything goes over the garbage *)
  type t

  (* O(1) *)
  val empty : unit -> t

  (* O(1) *)
  val is_empty : t -> bool

  (* O(1) *)
  val level_removed : Level_data.t -> t -> t

  (* O(1) *)
  val commit_collected : t -> t

  (* O(1) *)
  val commit_removed : Commit_data.t -> t -> t

  (* O(1) *)
  val game_collected : t -> t
end = struct
  type t = {
    commits : nat;
    games : nat;
  }

  let empty () = { commits = [%nat 0]; games = [%nat 0] }

  let is_empty t =
    let { commits; games } = t in
    commits = [%nat 0] && games = [%nat 0]

  let level_removed level_data t =
    let { commits; games } = t in
    let commits = commits + Level_data.commits level_data in
    { commits; games }
  let commit_collected t =
    let { commits; games } = t in
    (* TODO: should I check the property below somehow? *)
    let commits = abs (commits - [%nat 1]) in
    { commits; games }

  let commit_removed commit_data t =
    let { commits; games } = t in
    let games = games + Commit_data.games commit_data in
    { commits; games }
  let game_collected t =
    let { commits; games } = t in
    (* TODO: should I check the property below somehow? *)
    let games = abs (games - [%nat 1]) in
    { commits; games }
end

module Collateral_vault : sig
  type t

  (* O(1) *)
  val empty : unit -> t

  (* O(log2 length) *)
  val has_stake : address -> t -> bool

  (* O(log2 length) *)
  val join : address -> t -> t

  (* O(log2 length) *)
  val burn : address -> t -> t
end = struct
  type t = (address, unit) big_map

  let empty () : t = Big_map.empty

  let has_stake address (t : t) = Big_map.mem address t

  let join address (t : t) = Big_map.add address () t

  let burn address (t : t) = Big_map.remove address t
end

type t = {
  (* open levels *)
  (* TODO: should we enforce uniqueness on commits and games.
           Complexity: two honest can lead to two different games *)
  garbage : Garbage.t;
  levels : (level, Level_data.t) big_map;
  commits : (level * committer, Commit_data.t) big_map;
  games : (level * committer * rejector, Temporal_rejection_game.t) big_map;
  (* closed levels *)
  (* TODO: should this be a tuple state_hash * level *)
  trusted_state_hash : state_hash;
  trusted_level : level;
  (* subscription *)
  collateral_vault : Collateral_vault.t;
}

(* TODO: this module fetchs data twice for no good reason
         such as commit_data that was already fetched externally *)

let make ~initial_trusted_state_hash ~initial_trusted_level =
  {
    garbage = Garbage.empty ();
    levels = Big_map.empty;
    commits = Big_map.empty;
    games = Big_map.empty;
    trusted_state_hash = initial_trusted_state_hash;
    trusted_level = initial_trusted_level;
    collateral_vault = Collateral_vault.empty ();
  }
let has_garbage t = not (Garbage.is_empty t.garbage)

let append_action ~current_level ~action_hash t =
  let { garbage; levels; _ } = t in
  let level_data =
    match Big_map.find_opt current_level levels with
    | Some level_data -> level_data
    | None -> Level_data.empty () in

  let level_data = Level_data.action_added action_hash level_data in
  let levels = Big_map.add current_level level_data levels in
  { t with garbage; levels }

let find_level ~level t =
  (* TODO: if there is no action, we still require a published commit *)
  match Big_map.find_opt level t.levels with
  | Some level_data -> level_data
  | None -> Level_data.empty ()

let remove_level ~level t =
  let { garbage; levels; _ } = t in
  match Big_map.find_opt level levels with
  | None -> None
  | Some level_data ->
    let garbage = Garbage.level_removed level_data garbage in
    let levels = Big_map.remove level levels in

    Some { t with garbage; levels }

let append_commit ~level ~committer ~previous_state_hash ~state_hash ~steps t =
  let { levels; commits; _ } = t in
  match Big_map.find_opt (level, committer) commits with
  | Some _commit_data -> None
  | None ->
    let levels =
      let level_data =
        match Big_map.find_opt level levels with
        | Some level_data -> level_data
        | None -> Level_data.empty () in
      let level_data = Level_data.commit_added level_data in
      Big_map.add level level_data levels in

    let commit_data = Commit_data.make ~previous_state_hash ~state_hash ~steps in
    let commits = Big_map.add (level, committer) commit_data commits in

    Some { t with levels; commits }

let find_commit ~level ~committer t =
  let { levels; commits; _ } = t in
  if Big_map.mem level levels then
    Big_map.find_opt (level, committer) commits
  else
    None

let remove_commit ~level ~committer t =
  let { garbage; levels; commits; _ } = t in
  let commit_key = (level, committer) in
  match Big_map.find_opt commit_key commits with
  | None -> None
  | Some commit_data ->
  match Big_map.find_opt level levels with
  | None -> None
  | Some level_data ->
    let garbage = Garbage.commit_removed commit_data garbage in
    let levels =
      let level_data = Level_data.commit_removed level_data in
      Big_map.add level level_data levels in
    let commits = Big_map.remove commit_key commits in

    Some { t with garbage; levels; commits }

let collect_commit ~level ~committer t =
  let { garbage; levels; commits; _ } = t in
  let commit_key = (level, committer) in
  match Big_map.find_opt commit_key commits with
  | None -> None
  | Some commit_data ->
    if Big_map.mem level levels then
      None
    else
      let garbage =
        let garbage = Garbage.commit_collected garbage in
        Garbage.commit_removed commit_data garbage in
      let commits = Big_map.remove commit_key commits in

      Some { t with garbage; levels; commits }

let append_game ~level ~committer ~rejector game t =
  let { commits; games; _ } = t in
  let commit_key = (level, committer) in
  let game_key = (level, committer, rejector) in
  match Big_map.find_opt game_key games with
  | Some _game -> None
  | None ->
  match Big_map.find_opt commit_key commits with
  | None -> None
  | Some commit_data ->
    let commits =
      let commit_data = Commit_data.game_added commit_data in
      Big_map.add commit_key commit_data commits in
    let games = Big_map.add game_key game games in

    Some { t with commits; games }

let update_game ~level ~committer ~rejector game t =
  let { games; _ } = t in
  let game_key = (level, committer, rejector) in
  if Big_map.mem game_key games then
    let games = Big_map.add game_key game games in
    Some { t with games }
  else
    None

let find_game ~level ~committer ~rejector t =
  match find_commit ~level ~committer t with
  | Some _ -> Big_map.find_opt (level, committer, rejector) t.games
  | None -> None

let remove_game ~level ~committer ~rejector t =
  let { commits; games; _ } = t in
  let commit_key = (level, committer) in
  let game_key = (level, committer, rejector) in
  if Big_map.mem game_key games then
    match Big_map.find_opt commit_key commits with
    | None -> None
    | Some commit_data ->
      let commits =
        let commit_data = Commit_data.game_removed commit_data in
        Big_map.add commit_key commit_data commits in
      let games = Big_map.remove game_key games in

      Some { t with commits; games }
  else
    None

let collect_game ~level ~committer ~rejector t =
  let { garbage; commits; games; _ } = t in
  let commit_key = (level, committer) in
  let game_key = (level, committer, rejector) in
  if Big_map.mem game_key games then
    if Big_map.mem commit_key commits then
      None
    else
      let garbage = Garbage.game_collected garbage in
      let games = Big_map.remove game_key games in

      Some { t with garbage; commits; games }
  else
    None

let trust_level ~level t = { t with trusted_level = level }
let trusted_level t = t.trusted_level

let trust_state_hash state_hash t = { t with trusted_state_hash = state_hash }
let trusted_state_hash t = t.trusted_state_hash

let has_stake address t =
  let { collateral_vault; _ } = t in
  Collateral_vault.has_stake address collateral_vault

let join address t =
  let { collateral_vault; _ } = t in
  let collateral_vault = Collateral_vault.join address collateral_vault in
  { t with collateral_vault }

let burn address t =
  let { collateral_vault; _ } = t in
  let collateral_vault = Collateral_vault.burn address collateral_vault in
  { t with collateral_vault }
