open Tezos_environment

type player = Rejector | Committer

type state_hash = bytes

type level = nat

type steps = nat

type committer = address

type rejector = address

let current_level = Tezos.level

let _sender = Tezos.sender

let round_time = 10n

module Turn = struct
  (* A turn is a small step of a round *)
  type t = nat

  let current level =
    (* TODO: this is not needed for the smart contract *)
    let () = assert (current_level > level) in
    let levels_since_level = abs (current_level - level) in
    match ediv levels_since_level round_time with
    | Some (turn, _remainder) -> turn
    | None -> assert false

  type turn_kind = TCommitter | Fork_committer | TRejector | Fork_rejector

  let turns_per_round = 4n

  let turn_kind level =
    let current_turn = current level in
    (* C -> F_C -> R -> F_R *)
    match ediv current_turn turns_per_round with
    | Some (_, remainder) ->
        if remainder = 0n then TCommitter
        else if remainder = 1n then Fork_committer
        else if remainder = 2n then TRejector
        else Fork_rejector
    | None -> assert false
end

module Vm = struct
  type _hash = bytes

  module Pool = struct
    type t = (nat * _hash) list

    let empty_hash () = Crypto.blake2b (Bytes.pack ([] : nat list))

    let empty () : t = []

    let _hash (t : t) =
      match t with [] -> empty_hash () | (_el, _hash) :: _ -> _hash

    let push (el : nat) (t : t) : t =
      let _hash_of_t = _hash t in
      let _hash_of_el = Crypto.blake2b (Bytes.pack el) in
      let _hash = Crypto.blake2b (Bytes.concat _hash_of_el _hash_of_t) in
      (el, _hash) :: t

    let pop (t : t) = match t with [] -> None | (el, _) :: tl -> Some (el, tl)

    let is_empty (t : t) = match t with [] -> true | _ -> false

    let single_step_data (t : t) : t option =
      match t with [] -> None | el :: _tl -> Some [ el ]
  end

  (* TODO: attack vector, nat is unbounded, so very large state *)
  type t = { level : nat; steps : nat; counter : nat; pool : Pool.t }

  let single_step_data t =
    let { level; steps; counter; pool } = t in
    match Pool.single_step_data pool with
    | Some pool -> Some { level; steps; counter; pool }
    | None -> None

  let halted t = Pool.is_empty t.pool

  let execute_step t =
    let { level; steps; counter; pool } = t in
    match Pool.pop pool with
    | None ->
        (* can only happen if not enough data was sent *)
        assert false
    | Some (el, pool) ->
        let steps = steps + 1n in
        let counter = el + counter in
        { level; steps; counter; pool }

  let _hash (t : t) =
    let steps = t.steps in
    let counter = t.counter in
    let pool = Pool._hash t.pool in
    let data = (steps, (counter, pool)) in
    Crypto.blake2b (Bytes.pack data)

  let steps t = t.steps

  let apply nats t =
    let { level; steps = _; counter; pool } = t in
    let level = level + 1n in
    let steps = 0n in

    let pool =
      List.fold_left (fun (pool, nat) -> Pool.push nat pool) pool nats
    in
    let pool = Pool.push 0n pool in
    let pool = Pool.push 0n pool in

    { level; steps; counter; pool }
end

module Collateral_vault = struct
  type t = (address, unit) big_map

  let has_stake (address : address) (t : t) = Big_map.mem address t

  let join (address : address) (t : t) = Big_map.add address () t

  let burn (address : address) (t : t) = Big_map.remove address t
end

module Rejection_game = struct
  type vote = Agree | Disagree

  module Small_rejection_game = struct
    (* TODO: if we require commit to include steps we can skip a couple turns *)
    (* TODO: if we require the committer to send the _hash, we can fuse turns *)
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
      | To_replay of { state_hash : state_hash; expected : state_hash * player }
      | Winner of player

    type action = Send_hash of state_hash | Vote of vote | Replay of Vm.t

    (* TODO: a game can only have a single fork per level *)
    (* let find_game : game_id -> game = assert false *)

    let calculate_mid_step initial_step final_step =
      let diff = abs (final_step - initial_step) in
      match ediv diff 2n with
      | Some (mid_step, _remainder) -> mid_step
      | None -> assert false

    let start previous_state_hash committer rejector mid_state_hash =
      let committer_state_hash, committer_steps = committer in
      let rejector_state_hash, rejector_steps = rejector in
      let final =
        if committer_steps > rejector_steps then
          (rejector_state_hash, rejector_steps, Rejector)
        else (committer_state_hash, committer_steps, Committer)
      in
      let initial_step = 0n in
      let initial = (previous_state_hash, initial_step) in
      let _final_hash, final_step, _final_player = final in

      (* WARNING: *)
      (* VM steps needs to be bigger than 2n*)
      (* TODO: slash instead of fail *)
      let () = assert (final_step >= 2n) in
      let mid =
        let mid_step = calculate_mid_step initial_step final_step in
        (mid_state_hash, mid_step)
      in
      Vote_on_midpoint { initial; mid; final }

    (* TODO: proof showing that committer always agrees with initial _hash *)
    (* TODO: proof showing that committer almost always disagrees with final _hash *)

    let has_winner state =
      match state with Winner player -> Some player | _ -> None

    let play player action state =
      match (state, player, action) with
      | Waiting_midpoint { initial; final }, Rejector, Send_hash mid_hash ->
          let _initial_hash, initial_step = initial in
          let _final_hash, final_step, _expecting = final in
          let mid_step = calculate_mid_step initial_step final_step in
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
          if initial_step + 1n = final_step then
            To_replay
              { state_hash = initial_hash; expected = (final_hash, expecting) }
          else Waiting_midpoint { initial; final }
      | To_replay { state_hash; expected }, Rejector, Replay state ->
          (* TODO: slash instead of fail *)
          let () = assert (Vm._hash state = state_hash) in
          let state_ = Vm.execute_step state in

          let expected_hash, expected_author = expected in
          let opposite_player player =
            match player with Rejector -> Committer | Committer -> Rejector
          in
          let winner =
            if Vm._hash state_ = expected_hash then expected_author
            else opposite_player expected_author
          in

          Winner winner
      | _ ->
          (* TODO: slash instead of fail *)
          assert false
  end

  type state = {
    (* duplicated *)
    level : level;
    last_turn : Turn.t;
    (* state *)
    previous_state : Small_rejection_game.state option;
    current_state : Small_rejection_game.state;
  }

  type t = state

  type defend = Vote2 of vote | Timeout

  type attack = Mid_hash of state_hash | Replay2 of Vm.t | Timeout

  let make level state =
    let current_turn = Turn.current level in
    {
      level;
      last_turn = current_turn;
      previous_state = None;
      current_state = state;
    }

  let start level previous_state_hash committer rejector mid_state_hash =
    let state =
      Small_rejection_game.start previous_state_hash committer rejector
        mid_state_hash
    in
    make level state

  let current_turn state = Turn.current state.level

  let turn_kind state = Turn.turn_kind state.level

  let assert_is_expected_player player state =
    (* TODO: slash instead of fail *)
    match (turn_kind state, player) with
    | TCommitter, Committer -> ()
    | TRejector, Rejector -> ()
    | _ -> failwith "not your turn"

  (* timeout() -> remove_game | remove_commit *)

  let assert_can_claim_timeout player state =
    let current_turn = current_turn state in
    (* TODO: duplicatead Turn.current *)
    let () = assert_is_expected_player player state in
    assert (current_turn = state.last_turn + Turn.turns_per_round)

  let play player action state =
    let { level; last_turn; previous_state = _; current_state } = state in
    let previous_state = Some current_state in
    let current_state = Small_rejection_game.play player action current_state in
    { level; last_turn; previous_state; current_state }

  type move_result = Winner of player | Waiting of state

  let defend move state =
    match move with
    | Vote2 vote -> Waiting (play Committer (Vote vote) state)
    | Timeout ->
        let () = assert_can_claim_timeout Committer state in
        Winner Committer

  let attack move state =
    let current_turn = current_turn state in
    let state =
      if state.last_turn = current_turn then state
      else
        let () = assert (turn_kind state = TRejector) in
        {
          state with
          last_turn = current_turn;
          previous_state = Some state.current_state;
        }
    in
    match move with
    | Mid_hash _hash -> Waiting (play Rejector (Send_hash _hash) state)
    | Replay2 vm_state -> (
        let state = play Rejector (Replay vm_state) state in
        match Small_rejection_game.has_winner state.current_state with
        | Some player -> Winner player
        | None -> assert false)
    | Timeout ->
        let () = assert_can_claim_timeout Rejector state in
        Winner Rejector

  let fork state =
    (* TODO: do we care about last_turn *)
    let { level; last_turn = _; previous_state; current_state = _ } = state in
    let () = assert (Turn.turn_kind level = Fork_rejector) in

    match previous_state with
    | Some state -> Some (make level state)
    | None -> None
end

module Rejection_lazy_map = struct
  type t = { length : nat; items : (rejector, Rejection_game.t) big_map }

  let empty () = { length = 0n; items = Big_map.empty }

  let append rejector game t =
    if Big_map.mem rejector t.items then None
    else
      let length = t.length + 1n in
      let items = Big_map.add rejector game t.items in
      Some { length; items }

  let update rejector game t =
    if Big_map.mem rejector t.items then
      let items = Big_map.add rejector game t.items in
      Some { length = t.length; items }
    else None

  let remove rejector t =
    (* TODO: assert properties required to remove a rejection game *)
    if Big_map.mem rejector t.items then
      let length = abs (t.length - 1n) in
      let items = Big_map.remove rejector t.items in
      Some { length; items }
    else None

  let find rejector t = Big_map.find_opt rejector t.items

  let length t = t.length
end

module Commit = struct
  type t = {
    (* constants *)
    parent_state_hash : state_hash;
    state_hash : state_hash;
    (* TODO: validate steps is inside of the commit_hash *)
    steps : steps;
    (* duplicated *)
    level : level;
    (* last_turn committer made a movement *)
    last_turn : Turn.t;
    (* state *)
    previous_rejections : Rejection_lazy_map.t option;
    rejections : Rejection_lazy_map.t;
  }

  let current_turn t = Turn.current t.level

  let turn_kind t = Turn.turn_kind t.level

  let make level parent_state_hash state_hash steps =
    let current_turn = Turn.current level in
    {
      parent_state_hash;
      state_hash;
      steps;
      level;
      last_turn = current_turn;
      previous_rejections = None;
      rejections = Rejection_lazy_map.empty ();
    }

  let parent_state_hash t = t.parent_state_hash

  let state_hash t = t.state_hash

  let steps t = t.steps

  let rejections t = Rejection_lazy_map.length t.rejections

  let append_game rejector game t =
    match Rejection_lazy_map.append rejector game t.rejections with
    | Some rejections -> Some { t with rejections }
    | None -> None

  let remove_game rejector t =
    match Rejection_lazy_map.remove rejector t.rejections with
    | Some rejections -> Some { t with rejections }
    | None -> None

  let find_game rejector t = Rejection_lazy_map.find rejector t.rejections

  let update_game rejector game t =
    match Rejection_lazy_map.update rejector game t.rejections with
    | Some rejections -> Some { t with rejections }
    | None -> None

  type move_result = Committer_won of t | Rejector_won | Commit of t

  let claim_winner rejector winner commit =
    match winner with
    | Committer -> (
        match remove_game rejector commit with
        | Some commit -> Some (Committer_won commit)
        | None -> None)
    | Rejector -> Some Rejector_won

  let handle_move_result rejector (move_result : Rejection_game.move_result) t =
    match move_result with
    | Winner winner -> claim_winner rejector winner t
    | Waiting game -> (
        match update_game rejector game t with
        | Some t -> Some (Commit t)
        | None -> None)

  let defend rejector move t =
    let current_turn = current_turn t in
    let t =
      if t.last_turn = current_turn then t
      else
        let () = assert (turn_kind t = TCommitter) in
        {
          t with
          last_turn = current_turn;
          previous_rejections = Some t.rejections;
        }
    in

    match Rejection_lazy_map.find rejector t.rejections with
    | Some game ->
        handle_move_result rejector (Rejection_game.defend move game) t
    | None -> None

  let attack rejector move t =
    match Rejection_lazy_map.find rejector t.rejections with
    | Some game ->
        handle_move_result rejector (Rejection_game.attack move game) t
    | None -> None

  let fork t =
    let {
      parent_state_hash;
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
    let () = assert (Turn.turn_kind level = Fork_committer) in

    match previous_rejections with
    | Some rejections ->
        let t = make level parent_state_hash state_hash steps in
        Some { t with rejections }
    | None -> None
end

module Committer_lazy_map = struct
  type t = { length : nat; items : (committer, Commit.t) big_map }

  let empty () = { length = 0n; items = Big_map.empty }

  let append committer commit t =
    if Big_map.mem committer t.items then None
    else
      let length = t.length + 1n in
      let items = Big_map.add committer commit t.items in
      Some { length; items }

  let update committer commit t =
    (* TODO: this is unneeded but hmm *)
    if Big_map.mem committer t.items then
      let items = Big_map.add committer commit t.items in
      Some { length = t.length; items }
    else None

  let remove committer t =
    (* TODO: this is unneeded but hmm *)
    if Big_map.mem committer t.items then
      let length = abs (t.length - 1n) in
      let items = Big_map.remove committer t.items in
      Some { length; items }
    else None

  let find state_hash t = Big_map.find_opt state_hash t.items

  let mem state_hash t = Big_map.mem state_hash t.items

  let length t = t.length
end

(* THE IMPORTANT THING IS WE'RE BURNING SOMEONE'S MONEY *)
(*
  Our game, is the simple game, but lazy
  
  
  *)

(* IMPORTANT: the honest validator will never loose at anything *)
(* anyone can defend a commit *)

(* TODO: calculate worse scenarion, how much money honest needs *)
(* TODO: submit _hash only *)
type submission = bytes

type rejection = { operation_id : int; proof : bytes }

(* TODO: BIG TODO: go over all asserts and slash whoever is needed *)

(* TODO: put all required money to be a honest validator on the contract before starting rejections or commits *)

(* TODO: commits are allowed to also clean a level to avoid paying for increasing the storage *)

(* TODO: batch parameter to be more efficient in gas*)

(* TODO: split in two state machines, rejection game and optimistic rollup *)

let stake_amount : tez = assert false

let round_time = 10n

let time_to_respond = 1n * round_time

let time_to_timeout = 2n * round_time

let cooldown_period = 1n * round_time

(*
    Assume that C committed to something.
    The game goes like this:
    1. R starts the game by rejecting C's commitment, and gives a number of steps.
    2. C answers by giving the number of executions steps.
      The number of steps is agreed and it is the min of both.
      Let's say the number of steps = 1000.
    3. R gives h100, h200, h300, ... h1000
    4. C states the first one they disagree with
    5. Repeat 3&4 until we get a single _hash or the last one
    6. R gives the proof from h_n to h_n+1 and replays it
    7. We see who was correct between R and C
  *)

(* a fork only has effect if:

           G is the game that was forked
           G' is the new game based on G

           A is the player who's game turn was forked
           B is the player who forked G as A

           If A will lose G
           AND B wins G'

           The last person to fork and win, is always the honest validator
*)
(* you only fork if both sides are wrong *)

(* TODO: abstract this *)
type storage = {
  levels : (level, Committer_lazy_map.t) big_map;
  trusted : state_hash * level;
  collateral_vault : Collateral_vault.t; (* TODO: alive : bool; *)
}

type parameter =
  (* users *)
  | Submit of submission
  (* validators *)
  | Join
  | Exit
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
      (* TODO: ensure that a rejector is always using the same defend_as _hash *)
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
(* TODO? fuse commit *)

(* O(1) *)
let submit (_ : submission) (storage : storage) = storage

(* O(log2 collateral_vault) *)
let join (storage : storage) =
  let { levels; trusted; collateral_vault } = storage in
  let () = assert (Tezos.amount >= stake_amount) in
  let () = assert (not (Collateral_vault.has_stake _sender collateral_vault)) in
  let collateral_vault = Collateral_vault.join _sender collateral_vault in
  { levels; trusted; collateral_vault }

(* O(log2 collateral_vault) + transaction *)
let exit (storage : storage) =
  let { levels; trusted; collateral_vault } = storage in
  let () = assert (Collateral_vault.has_stake _sender collateral_vault) in
  let collateral_vault = Collateral_vault.burn _sender collateral_vault in

  let contract =
    match Tezos.get_contract_opt _sender with
    | Some contract -> contract
    | None -> failwith "failed to send your money back"
  in
  let transaction = Tezos.transaction () stake_amount contract in
  ([ transaction ], { levels; trusted; collateral_vault })

(* TODO: is it okay to accept commit(102) without having commit(101)?
    I think so, just the effects need to be ordered *)

(* open level is a level where new commits may be added *)
(* close level is a level where new commits cannot be added *)
(* finalized level is a closed level where no rejection can happen *)
(* TODO: trusted level is a finalized level with many conditions ... *)
(* dead level is a finalized level that is not trusted *)
let is_open_level (level : level) = level + time_to_respond >= current_level

let is_closed_level (level : level) = not (is_open_level level)

(*
  is closed level
  no rejection game going on
  last interaction must be older than a single round
  *)
(* let is_finalized_level (level : level) (storage : storage) =
   is_closed_level level
   &&
   match Big_map.find_opt level storage.levels with
   | Some level_data ->
       (* any level where there was no interaction for at least two rounds is finalized *)
       (* TODO: this could be a single round, if rejections were counted *)
       current_level >= level_data.last_interaction + (2n * round_time)
   | None -> true *)

let append_commit level commit committers storage =
  let committer = _sender in
  let { levels; trusted; collateral_vault } = storage in
  let committers =
    match Committer_lazy_map.append committer commit committers with
    | Some committers -> committers
    | None -> failwith "duplicated committer"
  in
  let levels = Big_map.add level committers levels in
  { levels; trusted; collateral_vault }

let commit level parent_state_hash state_hash steps storage =
  let { levels; trusted = _; collateral_vault } = storage in
  let () = assert (Collateral_vault.has_stake _sender collateral_vault) in
  (* TODO: magic number *)
  let () = assert (Turn.current level = 0n) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    | None -> Committer_lazy_map.empty ()
  in

  let commit = Commit.make level parent_state_hash state_hash steps in
  append_commit level commit committers storage

(* TODO: when can you fork? *)
let fork_commit level base_committer storage =
  let { levels; trusted = _; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake _sender collateral_vault) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    | None -> Committer_lazy_map.empty ()
  in

  let base_commit =
    match Committer_lazy_map.find base_committer committers with
    | Some commit -> commit
    | None -> failwith "this committer has no commit"
  in
  let commit =
    match Commit.fork base_commit with
    | Some commit -> commit
    | None -> failwith "a recently created commit cannot be forked"
  in
  append_commit level commit committers storage

let reject level committer defend_as mid_state_hash storage =
  (* TODO: prevent duplicated rejection game, if it matters *)
  let new_rejector = _sender in
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake new_rejector collateral_vault) in
  (* TODO: magic number *)
  let () = assert (Turn.current level = 2n) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    (* TODO: should this be a slash??? No data present *)
    | None -> Committer_lazy_map.empty ()
  in

  let committer_commit =
    match Committer_lazy_map.find committer committers with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid committer state _hash"
  in

  let rejector_commit =
    match Committer_lazy_map.find defend_as committers with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid rejector committer"
  in

  (* rejector agrees with parent_state_hash *)
  let () =
    assert (
      Commit.parent_state_hash committer_commit
      = Commit.parent_state_hash rejector_commit)
  in

  (* rejector disagrees with state_hash *)
  let () =
    assert (
      Commit.state_hash committer_commit <> Commit.state_hash rejector_commit)
  in

  let game =
    Rejection_game.start level
      (Commit.parent_state_hash committer_commit)
      (Commit.state_hash committer_commit, Commit.steps committer_commit)
      (Commit.state_hash rejector_commit, Commit.steps rejector_commit)
      mid_state_hash
  in

  let committer_commit =
    match Commit.append_game new_rejector game committer_commit with
    | Some commit -> commit
    | None -> failwith "duplicated rejectin game"
  in

  (* TODO: abstract this in Committer_lazy_map *)
  let committers =
    match Committer_lazy_map.update committer committer_commit committers with
    | Some committer -> committer
    | None -> failwith "unreachable"
  in
  let levels = Big_map.add level committers levels in

  { levels; trusted; collateral_vault }

let fork_reject level committer base_rejector storage =
  let rejector = _sender in
  let { levels; trusted; collateral_vault } = storage in

  (* TODO: remove duplicated code *)
  let () = assert (Collateral_vault.has_stake rejector collateral_vault) in

  (* TODO: prevent duplicating forking early if it matters *)
  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    (* TODO: should this be a slash??? No data present *)
    | None -> Committer_lazy_map.empty ()
  in

  let commit =
    match Committer_lazy_map.find committer committers with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid committer state _hash"
  in

  let base_game =
    match Commit.find_game base_rejector commit with
    | Some base_game -> base_game
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid rejector"
  in

  let game =
    match Rejection_game.fork base_game with
    | Some game -> game
    | None -> failwith "recently created game"
  in

  let commit =
    match Commit.append_game rejector game commit with
    | Some commit -> commit
    | None -> failwith "you already have a rejection game against this commit"
  in

  (* TODO: abstract this in Committer_lazy_map *)
  let committers =
    match Committer_lazy_map.update committer commit committers with
    | Some committer -> committer
    | None -> failwith "unreachable"
  in
  let levels = Big_map.add level committers levels in

  { levels; trusted; collateral_vault }

let handle_move_result committer (move_result : Commit.move_result option)
    committers =
  match move_result with
  | Some Rejector_won -> Committer_lazy_map.remove committer committers
  | Some (Committer_won commit) ->
      Committer_lazy_map.update committer commit committers
  | Some (Commit commit) ->
      Committer_lazy_map.update committer commit committers
  | None -> failwith "invalid rejector"

let slash_move_result committer rejector
    (move_result : Commit.move_result option) collateral_vault =
  match move_result with
  | Some (Committer_won _) -> Collateral_vault.burn rejector collateral_vault
  | Some Rejector_won -> Collateral_vault.burn committer collateral_vault
  | Some (Commit _) -> collateral_vault
  | None -> collateral_vault

let defend level rejector move storage =
  let committer = _sender in
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake committer collateral_vault) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    (* TODO: should this be a slash??? No data present *)
    | None -> Committer_lazy_map.empty ()
  in
  let commit =
    match Committer_lazy_map.find committer committers with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid committer state _hash"
  in

  let move_result = Commit.defend rejector move commit in
  let collateral_vault =
    slash_move_result committer rejector move_result collateral_vault
  in
  let committers =
    match handle_move_result committer move_result committers with
    | Some committers -> committers
    | None -> assert false
  in

  let levels = Big_map.add level committers levels in

  { levels; trusted; collateral_vault }

let attack level committer move storage =
  let rejector = _sender in
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake rejector collateral_vault) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    (* TODO: should this be a slash??? No data present *)
    | None -> Committer_lazy_map.empty ()
  in

  let commit =
    match Committer_lazy_map.find committer committers with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid committer state _hash"
  in
  let move_result = Commit.attack rejector move commit in
  let collateral_vault =
    slash_move_result committer rejector move_result collateral_vault
  in
  let committers =
    match handle_move_result committer move_result committers with
    | Some committers -> committers
    | None -> assert false
  in
  let levels = Big_map.add level committers levels in

  { levels; trusted; collateral_vault }

let trust_commit level committer storage =
  let { levels; trusted; collateral_vault } = storage in

  (* TODO: is this needed *)
  let () = assert (Collateral_vault.has_stake _sender collateral_vault) in

  let trusted_state_hash, trusted_level = trusted in
  let () = assert (level = trusted_level + 1n) in
  let () = assert (Turn.current level >= 3n) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    (* TODO: should this be a slash??? No data present *)
    | None -> Committer_lazy_map.empty ()
  in

  let commit =
    match Committer_lazy_map.find committer committers with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid committer state _hash"
  in

  let () = assert (Commit.parent_state_hash commit = trusted_state_hash) in
  let () = assert (Commit.rejections commit = 0n) in
  let trusted = (Commit.state_hash commit, Commit.steps commit) in
  let levels = Big_map.remove level levels in

  { levels; trusted; collateral_vault }

let main ((action, storage) : parameter * storage) =
  (* TODO: assert amount everywhere *)
  match action with
  | Submit submission ->
      let storage = submit submission storage in
      (([] : operation list), storage)
  | Join ->
      let storage = join storage in
      (([] : operation list), storage)
  | Exit -> exit storage
  | Commit { level; parent_state_hash; state_hash; steps } ->
      let storage = commit level parent_state_hash state_hash steps storage in
      (([] : operation list), storage)
  | Fork_commit { level; committer } ->
      let storage = fork_commit level committer storage in
      (([] : operation list), storage)
  | Reject { level; committer; defend_as; mid_state_hash } ->
      let storage = reject level committer defend_as mid_state_hash storage in
      (([] : operation list), storage)
  | Fork_reject { level; committer; rejector } ->
      let storage = fork_reject level committer rejector storage in
      (([] : operation list), storage)
  | Defend { level; rejector; move } ->
      let storage = defend level rejector move storage in
      (([] : operation list), storage)
  | Attack { level; committer; move } ->
      let storage = attack level committer move storage in
      (([] : operation list), storage)
  | Trust_commit { level; committer } ->
      let storage = trust_commit level committer storage in
      (([] : operation list), storage)

(* because the honest player is always playing the simple game,
   where they can defend their commit against everyone else and
   attack everyone else

   a dishonest player literally cannot do anything to change the outcome of a game on the game level
*)
