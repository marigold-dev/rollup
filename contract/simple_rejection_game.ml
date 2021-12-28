open Environment

(* TODO: instead of initial and final steps,
         it is possible to hold only the number of remaining steps

         so instead of storing 5-7 we store 2 *)
(* TODO: validate step is inside of state_hash, if it's not,
         which attack is possible? *)
type search_state = {
  initial_step : steps;
  initial_state_hash : state_hash;
  final_step : steps;
  final_state_hash : state_hash;
}
type handshake_state = {
  initial_state_hash : state_hash;
  final_step : steps;
  rejector_mid_state_hash : state_hash;
}
type searching_state = {
  search_state : search_state;
  committer_mid_state_hash : state_hash option;
  rejector_mid_state_hash : state_hash option;
}
type replay_state = {
  base_state_hash : state_hash;
  expected_state_hash : state_hash;
}
type state =
  | Handshake of handshake_state
  | Searching of searching_state
  | Replay of replay_state
type t = state

type move =
  | Move_handshake of { final_state_hash : state_hash }
  | Move_mid_state_hash of { mid_state_hash : state_hash }
  | Move_replay of { vm_state : Vm.t }

type move_result =
  | Move_result_winner of player
  | Move_result_waiting of state
  | Move_result_invalid

let search_state t =
  match t with
  | Handshake _ -> None
  | Searching
      {
        search_state;
        committer_mid_state_hash = _;
        rejector_mid_state_hash = _;
      } ->
      Some search_state
  | Replay _ -> None

let play ~previous_state_hash ~committer_steps ~committer_state_hash
    ~rejector_steps ~rejector_mid_state_hash =
  let () = assert (committer_steps >= 2n) in
  let () = assert (rejector_steps >= 2n) in

  if committer_steps > rejector_steps then
    Handshake
      {
        initial_state_hash = previous_state_hash;
        final_step = rejector_steps;
        rejector_mid_state_hash;
      }
  else
    Searching
      {
        search_state =
          {
            initial_step = 0n;
            initial_state_hash = previous_state_hash;
            final_step = committer_steps;
            final_state_hash = committer_state_hash;
          };
        committer_mid_state_hash = None;
        rejector_mid_state_hash = Some rejector_mid_state_hash;
      }

let move_handshake ~final_state_hash handshake =
  let { initial_state_hash; final_step; rejector_mid_state_hash } = handshake in
  let search_state =
    { initial_step = 0n; initial_state_hash; final_step; final_state_hash }
  in
  let state =
    Searching
      {
        search_state;
        committer_mid_state_hash = None;
        rejector_mid_state_hash = Some rejector_mid_state_hash;
      }
  in
  Move_result_waiting state

(* move_mid_state_hash *)
let find_mid_step ~initial_step ~final_step =
  let diff = abs (final_step - initial_step) in
  match ediv diff 2n with
  | Some (mid_step, _remainder) -> mid_step
  | None -> assert false

let step_search_state ~committer_mid_state_hash ~rejector_mid_state_hash
    search_state =
  let { initial_step; initial_state_hash; final_step; final_state_hash } =
    search_state
  in
  let mid_state_hash = committer_mid_state_hash in
  let mid_step = find_mid_step ~initial_step ~final_step in

  if committer_mid_state_hash = rejector_mid_state_hash then
    {
      initial_step = mid_step;
      initial_state_hash = mid_state_hash;
      final_step;
      final_state_hash;
    }
  else
    {
      initial_step;
      initial_state_hash;
      final_step = mid_step;
      final_state_hash = mid_state_hash;
    }
let finalize_turn ~committer_mid_state_hash ~rejector_mid_state_hash
    current_search_state =
  let search_state =
    step_search_state ~committer_mid_state_hash ~rejector_mid_state_hash
      current_search_state
  in

  if search_state.initial_step + 1n = search_state.final_step then
    let {
      initial_step = _;
      initial_state_hash = base_state_hash;
      final_step = _;
      final_state_hash = expected_state_hash;
    } =
      search_state
    in
    Replay { base_state_hash; expected_state_hash }
  else
    Searching
      {
        search_state;
        committer_mid_state_hash = None;
        rejector_mid_state_hash = None;
      }

let move_mid_state_hash player ~mid_state_hash searching =
  let { search_state; committer_mid_state_hash; rejector_mid_state_hash } =
    searching
  in

  (* TODO:
     should we punish players for trying to make duplicated move?
       a duplicated move should not put us in a different state anyway
       but slashing the player for a duplicated move implies some code
       complexity, while a failwith or a noop is very simple.
  *)
  let committer_mid_state_hash, rejector_mid_state_hash =
    match player with
    | Committer -> (Some mid_state_hash, rejector_mid_state_hash)
    | Rejector -> (committer_mid_state_hash, Some mid_state_hash)
  in
  let state =
    match (committer_mid_state_hash, rejector_mid_state_hash) with
    | Some committer_mid_state_hash, Some rejector_mid_state_hash ->
        finalize_turn ~committer_mid_state_hash ~rejector_mid_state_hash
          search_state
    | committer_mid_state_hash, rejector_mid_state_hash ->
        Searching
          { search_state; committer_mid_state_hash; rejector_mid_state_hash }
  in
  Move_result_waiting state

let move_replay vm_state replay_state =
  let { base_state_hash; expected_state_hash } = replay_state in

  if Vm.hash vm_state = base_state_hash then
    let vm_state = Vm.execute_step vm_state in
    let winner =
      if Vm.hash vm_state = expected_state_hash then Committer else Rejector
    in
    Move_result_winner winner
  else Move_result_invalid

let move player move state =
  match (player, move, state) with
  | Committer, Move_handshake { final_state_hash }, Handshake handshake ->
      move_handshake ~final_state_hash handshake
  | _, Move_mid_state_hash { mid_state_hash }, Searching searching_state ->
      move_mid_state_hash player ~mid_state_hash searching_state
  | _, Move_replay { vm_state }, Replay replay_state ->
      move_replay vm_state replay_state
  | _ -> Move_result_invalid
