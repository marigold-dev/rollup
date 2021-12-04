open Common

type pending_rejection_game =
  | Pending_Rejection_game of {
      level : level;
      accused_author : author;
      midpoint_hash : hash;
    }

module VM : sig
  type t
  val find_hash_at_step : step -> t -> hash
  val run_submissions :
    ?until_step:int -> submission list -> t -> t * step_count

  val hash_state : t -> hash
end = struct
  type t
  let find_hash_at_step step vm_state = assert false

  let run_submissions ?until_step submissions vm_state = assert false
  let hash_state = assert false
end

module Level_data : sig
  type t
  val initial : level -> VM.t -> t
  val add_level_data : block_data -> t -> t
  type trusted_data = private {
    hash : hash;
    vm_state : VM.t;
    step_count : step_count;
  }

  val find_trusted_data_for_level : level -> t -> trusted_data
  val find_submissions_for_level : level -> t -> submission list
  val find_endorsed_commit_opt : level -> t -> commit option
end = struct
  module Level_map = Map.Make (Int)
  type trusted_data = { hash : hash; vm_state : VM.t; step_count : step_count }
  type t = {
    submissions : submission list Level_map.t;
    commits : commit list Level_map.t;
    trusted_data : trusted_data Level_map.t;
    open_rejection_games : commit list Level_map.t;
  }
  let initial level vm_state =
    let commits = Level_map.empty |> Level_map.add level [] in
    let submissions = Level_map.empty |> Level_map.add level [] in
    let open_rejection_games = Level_map.empty |> Level_map.add level [] in
    let hash = VM.hash_state vm_state in
    let trusted_initial_level = { hash; vm_state; step_count = 0 } in
    let trusted_data =
      Level_map.empty |> Level_map.add level trusted_initial_level
    in
    { submissions; commits; trusted_data; open_rejection_games }

  let add_level_data block level_data =
    let { level; submissions = block_submissions; commits = block_commits } =
      block
    in
    let { commits; submissions; trusted_data; open_rejection_games; _ } =
      level_data
    in
    let submissions = Level_map.add level block_submissions submissions in
    let commits =
      List.fold_left
        (fun commits (level, commit) ->
          Level_map.update level
            (function
              | Some commits -> Some (commits @ [ commit ])
              | None -> Some [ commit ])
            commits)
        commits block_commits
    in
    let prev_trusted_data = Level_map.find (level - 1) trusted_data in
    let next_vm_state, next_step_count =
      VM.run_submissions block_submissions prev_trusted_data.vm_state
    in
    let next_trusted_data =
      {
        hash = VM.hash_state next_vm_state;
        vm_state = next_vm_state;
        step_count = next_step_count;
      }
    in
    let trusted_data = Level_map.add level next_trusted_data trusted_data in
    { submissions; commits; trusted_data; open_rejection_games }

  let find_trusted_data_for_level level { trusted_data; _ } =
    Level_map.find level trusted_data
  let find_submissions_for_level level { submissions; _ } =
    Level_map.find level submissions

  let find_endorsed_commit_opt level level_data =
    let { hash = trusted_hash; _ } =
      find_trusted_data_for_level level level_data
    in
    let level_commits = Level_map.find level level_data.commits in
    List.find_opt
      (fun (_author, hash, _step_count) -> hash = trusted_hash)
      level_commits
end

let find_new_pending_rejection_games commits level_data =
  List.fold_left
    (fun games (level, (committer, committed_hash, committed_step_count)) ->
      let Level_data.{ hash; vm_state; step_count } =
        Level_data.find_trusted_data_for_level level level_data
      in
      if hash = committed_hash && committed_step_count = step_count then games
      else
        let submissions =
          Level_data.find_submissions_for_level level level_data
        in
        let midpoint = List.length submissions / 2 in
        let midpoint_hash =
          VM.run_submissions ~until_step:midpoint submissions vm_state
          |> fst |> VM.hash_state
        in
        Pending_Rejection_game
          { level; midpoint_hash; accused_author = committer }
        :: games)
    [] commits

let promote_games pending_rejection_games level_data =
  List.fold_left
    (fun (promote_game_effects, pending_rejection_games)
         (Pending_Rejection_game { level; accused_author; midpoint_hash } as
         game) ->
      match Level_data.find_endorsed_commit_opt level level_data with
      | Some endorsed_commit ->
          let new_game_effect =
            Effect.Open_rejection_game
              { level; accused_author; midpoint_hash; endorsed_commit }
          in
          (new_game_effect :: promote_game_effects, pending_rejection_games)
      | None -> (promote_game_effects, game :: pending_rejection_games))
    ([], []) pending_rejection_games

type state = {
  level_data : Level_data.t;
  pending_rejection_games : pending_rejection_game list;
}
let transition : state -> block_data -> state * Effect.t list =
 fun { level_data; pending_rejection_games } block ->
  let level_data = Level_data.add_level_data block level_data in
  let Level_data.{ hash; vm_state = _; step_count } =
    Level_data.find_trusted_data_for_level block.level level_data
  in
  let commit_effect =
    Effect.Send_commit { level = block.level; hash; step_count }
  in
  let pending_rejection_games =
    find_new_pending_rejection_games block.commits level_data
    @ pending_rejection_games
  in
  let promote_game_effects, pending_rejection_games =
    promote_games pending_rejection_games level_data
  in
  let all_effects = commit_effect :: promote_game_effects in
  ({ level_data; pending_rejection_games }, all_effects)
