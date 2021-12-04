type level = int
type hash = bytes
type submission = bytes
type author = string
type step = int
type step_count = int
type commit = author * hash * step_count
type block_data = {
  level : level;
  submissions : submission list;
  commits : (level * commit) list;
}

module VM : sig
  type t
  val find_hash_at_step : step -> t -> hash
  val run_submissions : submission list -> t -> t * step_count
  val hash_state : t -> hash
end = struct
  type t
  let find_hash_at_step step vm_state = assert false

  let run_submissions submissions vm_state = assert false
  let hash_state = assert false
end

type effect =
  | Do_nothing
  | Send_commit of { level : level; hash : hash; step_count : step }
  | Open_rejection_game of {
      level : level;
      accused_author : author;
      midpoint_hash : hash;
      endorsed_commit : commit;
    }

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
  (* val find_hash_and_steps_for_level : level -> t -> hash * steps *)
  (* val find_submission_data_for_level : level -> t -> submission_data *)

  (* val steps_at_level : level -> t -> int *)
  (* val find_level_hash_at_step : level -> int -> t -> hash *)
  (* val find_last_level_hash : level -> t -> hash *)
end = struct
  module Level_map = Map.Make (Int)
  type trusted_data = { hash : hash; vm_state : VM.t; step_count : step_count }
  type t = {
    submissions : submission list Level_map.t;
    commits : commit list Level_map.t;
    trusted_data : trusted_data Level_map.t;
    rejection_games : commit list Level_map.t;
  }
  let initial level vm_state =
    let commits = Level_map.empty |> Level_map.add level [] in
    let submissions = Level_map.empty |> Level_map.add level [] in
    let rejection_games = Level_map.empty |> Level_map.add level [] in
    let hash = VM.hash_state vm_state in
    let trusted_initial_level = { hash; vm_state; step_count = 0 } in
    let trusted_data =
      Level_map.empty |> Level_map.add level trusted_initial_level
    in
    { submissions; commits; trusted_data; rejection_games }

  let add_level_data block level_data =
    let { level; submissions = block_submissions; commits = block_commits } =
      block
    in
    let { commits; submissions; trusted_data; rejection_games } = level_data in
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
    { submissions; commits; trusted_data; rejection_games }

  let find_trusted_data_for_level level { trusted_data; _ } =
    Level_map.find level trusted_data
end

module State_machine = struct
  type state = Level_data.t
  let transition : state -> block_data -> state * effect list =
   fun state block ->
    let state = Level_data.add_level_data block state in
    let Level_data.{ hash; vm_state; step_count } =
      Level_data.find_trusted_data_for_level block.level state
    in
    let commit_effect = Send_commit { level = block.level; hash; step_count } in
    let new_rejection_games =
      List.fold_left
        (fun games (level, (committer, committed_hash, committed_step_count)) ->
          let Level_data.{ hash; vm_state; step_count } =
            Level_data.find_trusted_data_for_level block.level state
          in
          if hash = committed_hash && committed_step_count = step_count then
            games
          else 
            let game = Open_rejection_game {
              level = block.level;
              accused_author = committer;
              endorsed_commit = 
            })
        [] block.commits
    in

    (* |> List.fold_left (fun (level, (author, hash, step_count) ->
           let trusted_data =
           Level_data.find_trusted_data_for_level block.level state
         in
         assert false
           )
         ) []
       in *)
    assert false
end
