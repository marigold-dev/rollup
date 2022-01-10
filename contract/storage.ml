open Environment

module Level_data : sig
  type t

  (* O(1) *)
  val empty : unit -> t

  (* O(1) *)
  val commits : t -> nat

  (* O(1) *)
  val commit_added : t -> t

  (* O(1) *)
  val commit_removed : t -> t
end = struct
  type commits = nat

  type t = commits

  let empty () = 0n

  let commits t = t

  let commit_added t = t + 1n
  let commit_removed t = abs (t - 1n)
end

module Commit_data : sig
  type t

  (* O(1) *)
  val make :
    parent_state_hash:state_hash -> state_hash:state_hash -> steps:steps -> t

  (* O(1) *)
  val parent_state_hash : t -> state_hash

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
    parent_state_hash : state_hash;
    state_hash : state_hash;
    (* TODO: validate steps is inside of the state_hash *)
    steps : steps;
    games : nat;
  }

  let make ~parent_state_hash ~state_hash ~steps =
    { parent_state_hash; state_hash; steps; games = 0n }

  let parent_state_hash t = t.parent_state_hash
  let state_hash t = t.state_hash
  let steps t = t.steps
  let games t = t.games

  let game_added t = { t with games = t.games + 1n }
  let game_removed t = { t with games = abs (t.games - 1n) }
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

  let empty () = { commits = 0n; games = 0n }

  let is_empty t =
    let { commits; games } = t in
    commits = 0n && games = 0n

  let level_removed level_data t =
    let { commits; games } = t in
    let commits = commits + Level_data.commits level_data in
    { commits; games }
  let commit_collected t =
    let { commits; games } = t in
    (* TODO: should I check the property below somehow? *)
    let commits = abs (commits - 1n) in
    { commits; games }

  let commit_removed commit_data t =
    let { commits; games } = t in
    let games = games + Commit_data.games commit_data in
    { commits; games }
  let game_collected t =
    let { commits; games } = t in
    (* TODO: should I check the property below somehow? *)
    let games = abs (games - 1n) in
    { commits; games }
end

type game
type t = {
  garbage : Garbage.t;
  levels : (level, Level_data.t) big_map;
  commits : (level * committer, Commit_data.t) big_map;
  games : (level * committer * rejector, game) big_map;
}

(* TODO: this module fetchs data twice for no good reason
         such as commit_data that was already fetched externally *)

let empty () =
  {
    garbage = Garbage.empty ();
    levels = Big_map.empty;
    commits = Big_map.empty;
    games = Big_map.empty;
  }
let has_garbage t = not (Garbage.is_empty t.garbage)

let remove_level ~level t =
  let { garbage; levels; commits; games } = t in
  match Big_map.find_opt level levels with
  | None -> None
  | Some level_data ->
    let garbage = Garbage.level_removed level_data garbage in
    let levels = Big_map.remove level levels in

    Some { garbage; levels; commits; games }

let append_commit ~level ~committer ~commit_data t =
  let { garbage; levels; commits; games } = t in
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
    let commits = Big_map.add (level, committer) commit_data commits in

    Some { garbage; levels; commits; games }

let remove_commit ~level ~committer t =
  let { garbage; levels; commits; games } = t in
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

    Some { garbage; levels; commits; games }

let collect_commit ~level ~committer t =
  let { garbage; levels; commits; games } = t in
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

      Some { garbage; levels; commits; games }

let append_game ~level ~committer ~rejector game t =
  let { garbage; levels; commits; games } = t in
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

    Some { garbage; levels; commits; games }

let remove_game ~level ~committer ~rejector t =
  let { garbage; levels; commits; games } = t in
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

      Some { garbage; levels; commits; games }
  else
    None

let collect_game ~level ~committer ~rejector t =
  let { garbage; levels; commits; games } = t in
  let commit_key = (level, committer) in
  let game_key = (level, committer, rejector) in
  if Big_map.mem game_key games then
    if Big_map.mem commit_key commits then
      None
    else
      let garbage = Garbage.game_collected garbage in
      let games = Big_map.remove game_key games in

      Some { garbage; levels; commits; games }
  else
    None
