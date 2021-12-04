open Common

let ( >>= ) = Option.bind

type pending_rejection_game =
  | Pending_Rejection_game of {
      level : level;
      accused_author : author;
      midpoint_hash : hash;
    }

let self =
  (* TODO: get this from config *)
  "myself"

module Level_data : sig
  type t
  type trusted_data = private {
    hash : hash;
    vm_state : Vm.t;
    step_count : step_count;
  }
  val initial : level -> Vm.t -> t
  val add_level_data : block_data -> t -> t
  val find_trusted_data_for_level : level -> t -> trusted_data
  val find_submissions_for_level : level -> t -> submission list
  val find_endorsed_commit : level -> t -> commit
end = struct
  module Level_map = Map.Make (Int)
  type trusted_data = { hash : hash; vm_state : Vm.t; step_count : step_count }
  type t = {
    current_block_height : int;
    submissions : submission list Level_map.t;
    commits : commit list Level_map.t;
    trusted_data : trusted_data Level_map.t;
    open_rejection_games : commit list Level_map.t;
  }
  let initial level vm_state =
    let commits = Level_map.empty |> Level_map.add level [] in
    let submissions = Level_map.empty |> Level_map.add level [] in
    let open_rejection_games = Level_map.empty |> Level_map.add level [] in
    let hash = Vm.hash_state vm_state in
    let trusted_initial_level = { hash; vm_state; step_count = 0 } in
    let trusted_data =
      Level_map.empty |> Level_map.add level trusted_initial_level
    in
    {
      current_block_height = level;
      submissions;
      commits;
      trusted_data;
      open_rejection_games;
    }

  let add_level_data block level_data =
    let { level; submissions = block_submissions; commits = block_commits } =
      block
    in
    let {
      commits;
      submissions;
      trusted_data;
      open_rejection_games;
      current_block_height;
    } =
      level_data
    in
    if level <> current_block_height + 1 then
      failwith "You must add each block in order"
    else
      let submissions = Level_map.add level block_submissions submissions in
      let commits =
        List.fold_left
          (fun commits (Commit commit) ->
            Level_map.update commit.commit_level
              (function
                | Some commits -> Some (commits @ [ Commit commit ])
                | None -> Some [ Commit commit ])
              commits)
          commits block_commits
      in
      let prev_trusted_data = Level_map.find (level - 1) trusted_data in
      let next_vm_state, next_step_count =
        Vm.run_submissions block_submissions prev_trusted_data.vm_state
      in
      let next_trusted_data =
        {
          hash = Vm.hash_state next_vm_state;
          vm_state = next_vm_state;
          step_count = next_step_count;
        }
      in
      let trusted_data = Level_map.add level next_trusted_data trusted_data in
      {
        current_block_height = level;
        submissions;
        commits;
        trusted_data;
        open_rejection_games;
      }

  let find_trusted_data_for_level level { trusted_data; _ } =
    Level_map.find level trusted_data
  let find_submissions_for_level level { submissions; _ } =
    Level_map.find level submissions

  let find_endorsed_commit level level_data =
    let { hash = trusted_hash; _ } =
      find_trusted_data_for_level level level_data
    in
    let level_commits = Level_map.find level level_data.commits in
    let my_commit =
      List.find_opt (fun (Commit { author; _ }) -> author = self) level_commits
    in
    match my_commit with
    | Some my_commit -> my_commit
    | None ->
        List.find
          (fun (Commit { hash; _ }) -> hash = trusted_hash)
          level_commits
end

let make_new_rejection_games commits level_data =
  List.fold_left
    (fun games
         (Commit
           {
             commit_level;
             author;
             hash = committed_hash;
             step_count = committed_step_count;
           }) ->
      let Level_data.{ hash; vm_state; step_count } =
        Level_data.find_trusted_data_for_level commit_level level_data
      in
      if hash = committed_hash && committed_step_count = step_count then games
      else
        let submissions =
          Level_data.find_submissions_for_level commit_level level_data
        in
        let midpoint = List.length submissions / 2 in
        let midpoint_hash =
          Vm.run_submissions ~until_step:midpoint submissions vm_state
          |> fst |> Vm.hash_state
        in
        let endorsed_commit =
          Level_data.find_endorsed_commit commit_level level_data
        in
        Effect.Open_rejection_game
          {
            level = commit_level;
            midpoint_hash;
            accused_author = author;
            endorsed_commit;
          }
        :: games)
    [] commits

type state = Level_data.t

let transition : state -> block_data -> state * Effect.t list =
 fun state block ->
  let state = Level_data.add_level_data block state in
  let Level_data.{ hash; vm_state = _; step_count } =
    Level_data.find_trusted_data_for_level block.level state
  in
  let commit_effect =
    Effect.Send_commit { level = block.level; hash; step_count }
  in
  let rejection_game_effects = make_new_rejection_games block.commits state in
  let all_effects = commit_effect :: rejection_game_effects in
  (state, all_effects)
