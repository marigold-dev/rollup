open Environment

type vote = Agree | Disagree [@@deriving show { with_path = false }]

module Small_rejection_game = struct
  (* TODO: if we require commit to include steps we can skip a couple turns *)
  (* TODO: if we require the committer to send the hash, we can fuse turns *)
  (* TODO: remove committer if it looses as a rejector *)
  (* TODO: log2 of steps to know maximum of turns instead of holding each individual step, thank you Daniel*)

  type state =
    | Vote_on_midpoint of {
        initial : state_hash * steps;
        mid : state_hash * steps;
        final : state_hash * steps * player;
      }
    | Waiting_midpoint of {
        initial : state_hash * steps;
        final : state_hash * steps * player;
      }
    | To_replay of {
        step : steps;
        state_hash : state_hash;
        expected : state_hash * player;
      }
    | Winner of player
  [@@deriving show { with_path = false }]

  type action = Send_hash of state_hash | Vote of vote | Replay of Vm.t

  (* TODO: a game can only have a single fork per level *)
  (* let find_game : game_id -> game = assert false *)

  let calculate_mid_step ~initial_step ~final_step =
    let diff = abs (final_step - initial_step) in
    diff / Z.of_int 2

  let start ~previous_state_hash ~committer ~rejector ~mid_state_hash =
    let committer_state_hash, committer_steps = committer in
    let rejector_state_hash, rejector_steps = rejector in
    let final =
      if committer_steps > rejector_steps then
        (rejector_state_hash, rejector_steps, Rejector)
      else (committer_state_hash, committer_steps, Committer)
    in
    let initial_step = Z.zero in
    let initial = (previous_state_hash, initial_step) in
    let _final_hash, final_step, _final_player = final in

    (* WARNING: *)
    (* VM steps needs to be bigger than 2n*)
    (* TODO: slash instead of fail *)
    let () = assert (final_step >= Z.of_int 2) in
    let mid =
      let mid_step = calculate_mid_step ~initial_step ~final_step in
      (mid_state_hash, mid_step)
    in
    Vote_on_midpoint { initial; mid; final }

  (* TODO: proof showing that committer always agrees with initial hash *)
  (* TODO: proof showing that committer almost always disagrees with final hash *)

  let has_winner state =
    match state with Winner player -> Some player | _ -> None

  let play player action state =
    match (state, player, action) with
    | Waiting_midpoint { initial; final }, Rejector, Send_hash mid_hash ->
        let _initial_hash, initial_step = initial in
        let _final_hash, final_step, _expecting = final in
        let mid_step = calculate_mid_step ~initial_step ~final_step in
        let mid = (mid_hash, mid_step) in
        Vote_on_midpoint { initial; mid; final }
    | Vote_on_midpoint { initial; mid; final }, Committer, Vote vote ->
        let initial, final =
          match vote with
          | Agree -> (mid, final)
          | Disagree ->
              let mid_state_hash, mid_step = mid in
              let final = (mid_state_hash, mid_step, Rejector) in
              (initial, final)
        in

        let initial_hash, initial_step = initial in
        let final_hash, final_step, expecting = final in
        if initial_step + Z.one = final_step then
          To_replay
            {
              step = initial_step;
              state_hash = initial_hash;
              expected = (final_hash, expecting);
            }
        else Waiting_midpoint { initial; final }
    | To_replay { step = _; state_hash; expected }, Rejector, Replay state ->
        (* TODO: slash instead of fail *)
        let () = assert (Vm.hash state = state_hash) in
        let state' = Vm.execute_step state in

        let expected_hash, expected_author = expected in
        let opposite_player = function
          | Rejector -> Committer
          | Committer -> Rejector
        in
        let winner =
          if Vm.hash state' = expected_hash then expected_author
          else opposite_player expected_author
        in

        Winner winner
    | _ ->
        (* TODO: slash instead of fail *)
        assert false

  let rec execute_until_steps ~steps vm_state =
    if steps >= Vm.steps vm_state then vm_state
    else
      let vm_state = Vm.execute_step vm_state in
      execute_until_steps ~steps vm_state
end

open Small_rejection_game

type defend = Vote of vote | Timeout [@@deriving show { with_path = false }]
type attack = Mid_hash of state_hash | Replay of Vm.t | Timeout
[@@deriving show { with_path = false }]

type state = {
  (* duplicated *)
  level : level;
  last_turn : Turn.t;
  (* state *)
  previous_state : Small_rejection_game.state option;
  current_state : Small_rejection_game.state;
}
[@@deriving show { with_path = false }]
type t = state [@@deriving show { with_path = false }]

let find_defend ~initial_vm_state t : defend =
  match t.current_state with
  | Vote_on_midpoint { initial = _; mid; final = _ } ->
      let mid_hash, mid_step = mid in
      let mid_state = execute_until_steps ~steps:mid_step initial_vm_state in
      if Vm.hash mid_state = mid_hash then Vote Agree else Vote Disagree
  | _ -> Timeout
let find_mid_hash ~committer_steps ~rejector_steps ~initial_vm_state =
  let final_steps =
    if committer_steps > rejector_steps then rejector_steps else committer_steps
  in
  let mid_step =
    calculate_mid_step ~initial_step:Z.zero ~final_step:final_steps
  in
  let mid_state = execute_until_steps ~steps:mid_step initial_vm_state in
  Vm.hash mid_state

let find_attack ~initial_vm_state t : attack =
  match t.current_state with
  | Waiting_midpoint { initial; final } ->
      let _initial_hash, initial_step = initial in
      let _final_hash, final_step, _final_player = final in

      let mid_step = calculate_mid_step ~initial_step ~final_step in
      let mid_state = execute_until_steps ~steps:mid_step initial_vm_state in
      Mid_hash (Vm.hash mid_state)
  | To_replay { step; state_hash = _; expected = _ } ->
      let state = execute_until_steps ~steps:step initial_vm_state in
      let state = Vm.single_step_data state |> Option.get in
      Replay state
  | _ -> Timeout

let make ~current_level ~level state =
  let current_turn = Turn.current ~current_level ~level in
  {
    level;
    last_turn = current_turn;
    previous_state = None;
    current_state = state;
  }

let start ~current_level ~level ~previous_state_hash ~committer ~rejector
    ~mid_state_hash =
  let state = start ~previous_state_hash ~committer ~rejector ~mid_state_hash in
  make ~current_level ~level state

let current_turn ~current_level state =
  Turn.current ~current_level ~level:state.level

let play player action state =
  let { level; last_turn; previous_state = _; current_state } = state in
  let previous_state = Some current_state in
  let current_state = play player action current_state in
  { level; last_turn; previous_state; current_state }

type move_result = Winner of player | Waiting of state

let defend move state =
  match move with
  | Vote vote -> Waiting (play Committer (Vote vote) state)
  | Timeout -> Winner Committer

let attack ~current_level move state =
  let current_turn = current_turn ~current_level state in
  let state =
    if state.last_turn = current_turn then state
    else
      {
        state with
        last_turn = current_turn;
        previous_state = Some state.current_state;
      }
  in
  match move with
  | Mid_hash hash -> Waiting (play Rejector (Send_hash hash) state)
  | Replay vm_state -> (
      let state = play Rejector (Replay vm_state) state in
      match has_winner state.current_state with
      | Some player -> Winner player
      | None -> assert false)
  | Timeout -> Winner Rejector

let fork ~current_level state =
  (* TODO: do we care about last_turn *)
  let { level; last_turn = _; previous_state; current_state = _ } = state in

  match previous_state with
  | Some state -> make ~current_level ~level state
  | None -> assert false
