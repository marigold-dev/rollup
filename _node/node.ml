open Environment

let self =
  Address.of_string "tz1hTnFPNW4qFAzYvwHChA83B2dD18VTzvYJ" |> Option.get
type input_desc =
  | Submit of nat
  | Commit of {
      level : level;
      parent_state_hash : state_hash;
      state_hash : state_hash;
      steps : steps;
    }
  | Fork_commit of { level : level; committer : committer }
  | Reject of {
      level : level;
      committer : committer;
      (* TODO: ensure that a rejector is always using the same defend_as hash *)
      defend_as : committer;
      mid_state_hash : state_hash;
    }
  | Fork_reject of { level : level; committer : committer; rejector : rejector }
  | Defend of {
      level : level;
      rejector : rejector;
      move : Rejection_game.defend;
    }
  | Attack of {
      level : level;
      committer : committer;
      move : Rejection_game.attack;
    }
  | Trust_commit of { level : level; committer : committer }
[@@deriving show { with_path = false }]
type input = { sender : address; desc : input_desc }

type block = { level : level; operations : input list }

module Committer_map = Map_with_pp.Make (Address)
module Level_map = Map_with_pp.Make (Z)
module Committer_set = Set_with_pp.Make (Address)

type submission = nat [@@deriving show { with_path = false }]
type level_data = {
  (* input, this is reversed *)
  submissions : submission list;
  (* trusted *)
  parent_hash : state_hash;
  hash : state_hash;
  steps : steps;
  initial_state : Vm.t;
  (* disputes *)
  made_commit : bool;
  trusted_committers : Committer_set.t;
  last_trusted_committer : committer option;
  committers : Commit.t Committer_map.t;
}
[@@deriving show { with_path = false }]

type state = {
  current_level : level;
  trusted_level : level;
  last_state : Vm.t;
  last_hash : hash;
  levels : level_data Level_map.t;
}
[@@deriving show { with_path = false }]
let vm_state = Vm.initial ~level:Z.one

let rec execute_until_halts vm_state =
  if Vm.halted vm_state then vm_state
  else
    let vm_state = Vm.execute_step vm_state in
    execute_until_halts vm_state

let with_level level state f =
  match Level_map.find_opt level state.levels with
  | Some level_data ->
      let level_data = f level_data in
      let levels = Level_map.add level level_data state.levels in
      { state with levels }
  | None -> state

let handle_move_result ~committer (move_result : Commit.move_result) committers
    =
  match move_result with
  | Rejector_won -> Committer_map.remove committer committers
  | Commit commit -> Committer_map.add committer commit committers

let consume_input ~current_level submissions state input =
  let with_level level state f =
    let state = with_level level state f in
    (submissions, state)
  in
  match input.desc with
  | Submit nat -> (nat :: submissions, state)
  | Commit { level; parent_state_hash; state_hash; steps } ->
      let committer = input.sender in
      with_level level state (fun level_data ->
          let good_commit =
            level_data.parent_hash = parent_state_hash
            && level_data.hash = state_hash
            && level_data.steps = steps
          in
          let commit =
            Commit.make ~current_level ~level
              ~initial_vm_state:level_data.initial_state ~good_commit
              ~parent_state_hash ~state_hash ~steps
          in
          let committers =
            Committer_map.add input.sender commit level_data.committers
          in
          let trusted_committers, last_trusted_committer =
            if good_commit then
              ( Committer_set.add committer level_data.trusted_committers,
                Some committer )
            else
              (level_data.trusted_committers, level_data.last_trusted_committer)
          in
          {
            level_data with
            trusted_committers;
            last_trusted_committer;
            committers;
          })
  | Fork_commit { level; committer } ->
      with_level level state (fun level_data ->
          let base_commit =
            Committer_map.find committer level_data.committers
          in
          let commit = Commit.fork ~current_level base_commit in
          let committers =
            Committer_map.add input.sender commit level_data.committers
          in
          { level_data with committers })
  | Reject { level; committer; defend_as; mid_state_hash } ->
      let rejector = input.sender in
      with_level level state (fun level_data ->
          let committer_commit =
            Committer_map.find committer level_data.committers
          in
          let rejector_commit =
            Committer_map.find defend_as level_data.committers
          in

          let game =
            Rejection_game.start ~current_level ~level
              ~previous_state_hash:(Commit.parent_state_hash committer_commit)
              ~committer:
                ( Commit.state_hash committer_commit,
                  Commit.steps committer_commit )
              ~rejector:
                (Commit.state_hash rejector_commit, Commit.steps rejector_commit)
              ~mid_state_hash
          in

          let commit = Commit.add_game ~rejector game committer_commit in
          let committers =
            Committer_map.add input.sender commit level_data.committers
          in
          { level_data with committers })
  | Fork_reject { level; committer; rejector = base_rejector } ->
      let rejector = input.sender in
      with_level level state (fun level_data ->
          let commit = Committer_map.find committer level_data.committers in
          let base_game = Commit.find_game ~rejector:base_rejector commit in
          let game = Rejection_game.fork ~current_level base_game in
          let commit = Commit.add_game ~rejector game commit in
          let committers =
            Committer_map.add input.sender commit level_data.committers
          in
          { level_data with committers })
  | Defend { level; rejector; move } ->
      let committer = input.sender in
      with_level level state (fun level_data ->
          let committers = level_data.committers in
          let commit = Committer_map.find committer level_data.committers in

          let is_relevant =
            (not level_data.made_commit)
            && Committer_set.mem committer level_data.trusted_committers
          in
          let trusted_committers, last_trusted_committer =
            if is_relevant then
              let expected_move = Commit.find_defend ~rejector commit in
              if expected_move = move then
                ( level_data.trusted_committers,
                  level_data.last_trusted_committer )
              else
                ( Committer_set.remove committer level_data.trusted_committers,
                  Some committer )
            else
              (level_data.trusted_committers, level_data.last_trusted_committer)
          in
          let move_result =
            Commit.defend ~current_level ~rejector move commit
          in
          let committers =
            handle_move_result ~committer move_result committers
          in
          {
            level_data with
            trusted_committers;
            last_trusted_committer;
            committers;
          })
  | Attack { level; committer; move } ->
      let rejector = input.sender in
      with_level level state (fun level_data ->
          let committers = level_data.committers in
          let commit = Committer_map.find committer level_data.committers in

          let move_result =
            Commit.attack ~current_level ~rejector move commit
          in
          let committers =
            handle_move_result ~committer move_result committers
          in
          { level_data with committers })
  | Trust_commit { level; committer = _ } ->
      let levels = Level_map.remove level state.levels in
      (submissions, { state with levels; trusted_level = level })

let input_block block state =
  let submissions, state =
    List.fold_left
      (fun (submissions, state) input ->
        consume_input ~current_level:block.level submissions state input)
      ([], state) block.operations
  in

  let initial_state = Vm.apply submissions state.last_state in
  let last_state = execute_until_halts initial_state in
  let level_data =
    {
      submissions;
      hash = Vm.hash last_state;
      steps = Vm.steps last_state;
      initial_state;
      committers = Committer_map.empty;
      parent_hash = state.last_hash;
      made_commit = false;
      trusted_committers = Committer_set.empty;
      last_trusted_committer = None;
    }
  in
  let levels = Level_map.add block.level level_data state.levels in
  {
    state with
    current_level = block.level;
    last_state;
    last_hash = Vm.hash last_state;
    levels;
  }

let defend_moves commit =
  let level = Commit.level commit in
  Commit.find_defends commit
  |> List.map (fun (rejector, move) -> Defend { level; rejector; move })
let find_moves ~current_level ~level level_data =
  match Turn.turn_kind ~current_level ~level with
  | Committer ->
      if
        Committer_set.is_empty level_data.trusted_committers
        && not level_data.made_commit
      then
        let commit =
          Commit
            {
              level;
              parent_state_hash = level_data.parent_hash;
              state_hash = level_data.hash;
              steps = level_data.steps;
            }
        in
        let last_trusted_committer = Some self in
        let level_data =
          { level_data with made_commit = true; last_trusted_committer }
        in
        (level_data, [ commit ])
      else (level_data, [])
  | Fork_committer ->
      if
        Committer_set.is_empty level_data.trusted_committers
        && level_data.last_trusted_committer <> None
        && not level_data.made_commit
      then
        let base_committer = level_data.last_trusted_committer |> Option.get in
        let base_commit =
          Committer_map.find base_committer level_data.committers
        in
        let commit = Commit.fork ~current_level base_commit in
        let fork = Fork_commit { level; committer = base_committer } in
        let defends = defend_moves commit in
        let last_trusted_committer = Some self in
        let level_data =
          { level_data with made_commit = true; last_trusted_committer }
        in
        (level_data, fork :: defends)
      else (level_data, [])
  | Rejector ->
      let defend_as = level_data.last_trusted_committer |> Option.get in
      let committers, moves =
        Committer_map.fold
          (fun committer commit (committers, moves) ->
            let commit, additional_moves =
              if Commit.needs_rejector commit then
                let mid_state_hash =
                  Rejection_game.find_mid_hash
                    ~committer_steps:(Commit.steps commit)
                    ~rejector_steps:level_data.steps
                    ~initial_vm_state:level_data.initial_state
                in
                let reject =
                  Reject { level; committer; defend_as; mid_state_hash }
                in
                let commit = Commit.made_rejection commit in
                (commit, [ reject ])
              else (commit, [])
            in
            let committers = Committer_map.add committer commit committers in
            let moves = additional_moves @ moves in
            (committers, moves))
          level_data.committers (Committer_map.empty, [])
      in

      let level_data = { level_data with committers } in
      (level_data, moves)
  | Fork_rejector -> assert false
let all_moves ~current_level state =
  let levels, moves =
    Level_map.fold
      (fun level level_data (levels, moves) ->
        let level_data, additional_moves =
          find_moves ~current_level ~level level_data
        in
        let levels = Level_map.add level level_data levels in
        let moves = additional_moves @ moves in
        (levels, moves))
      state.levels (Level_map.empty, [])
  in
  ({ state with levels }, moves)

(* let moves state =
   state.levels |> Level_map.fold (fun level level_data moves -> ()) *)
let zero_state =
  let initial_vm = Vm.initial ~level:Z.zero in
  {
    current_level = Z.zero;
    trusted_level = Z.zero;
    last_state = initial_vm;
    last_hash = Vm.hash initial_vm;
    levels = Level_map.empty;
  }

let () = Format.printf "%a\n%!" pp_state zero_state

let a = Address.of_string "tz1hTnFPNW4qFAzYvwHChA83B2dD18VTzvYJ" |> Option.get
let a_input desc = { sender = a; desc }
let one_state =
  input_block
    { level = Z.one; operations = [ a_input (Submit Z.one) ] }
    zero_state
let () = Format.printf "%a\n%!" pp_state one_state
let one_state, one_moves = all_moves ~current_level:Z.one one_state
let () = Format.printf "%s\n%!" ([%show: input_desc list] one_moves)
let () = assert (List.length one_moves = 1)

let two_state =
  input_block
    {
      level = Z.of_int 2;
      operations =
        [
          a_input
            (Commit
               {
                 level = Z.one;
                 parent_state_hash = zero_state.last_hash;
                 state_hash = one_state.last_hash;
                 steps = Z.of_int 3;
               });
        ];
    }
    one_state
let two_state, two_moves = all_moves ~current_level:(Z.of_int 2) two_state
let () = Format.printf "%s\n%!" ([%show: input_desc list] two_moves)

let () = Format.printf "%a\n%!" pp_state two_state
