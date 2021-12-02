open Tezos_environment

(* IMPORTANT: the honest validator will never loose at anything *)
(* anyone can defend a commit *)
let current_level = Tezos.level

let sender = Tezos.sender

type level = nat
type step = nat

(* TODO: calculate worse scenarion, how much money honest needs *)
(* TODO: submit hash only *)
type submission = bytes
type state_hash = bytes
type rejection = { operation_id : int; proof : bytes }

(* TODO: put all required money to be a honest validator on the contract before starting rejections or commits *)

(* TODO: commits are allowed to also clean a level to avoid paying for increasing the storage *)

(* TODO: batch parameter to be more efficient in gas*)
type rejection_game_id = nat
type new_rejection_game = {
  level : level;
  (* TODO: this needs to be a known state_hash on this level *)
  committer_state_hash : state_hash;
  rejector_mid_state_hash : state_hash;
  rejector_state_hash : state_hash;
  rejector_steps : step;
}
type parameter =
  (* users *)
  | Submit of submission
  (* validators *)
  | Join
  | Exit
  | Commit of level * state_hash * step
  (* | Trust_commit of level * state_hash *)
  | Start_rejection_game of new_rejection_game
  | Send_middle_hash of rejection_game_id * state_hash

(* TODO: split in two state machines, rejection game and optimistic rollup *)

type commitments = {
  finalized_after : level;
  state_hashes : (state_hash, address) map;
}

let stake_amount : mutez = assert false
let commitment_amount : tez = (* 1000tz *) assert false

module Collateral_vault : sig
  type t

  (* O(log2 length) *)
  val has_stake : address -> t -> bool

  (* O(log2 length) *)
  val join : address -> t -> t

  (* O(log2 length) *)
  val burn : address -> t -> t
end = struct
  type t = (address, unit) big_map

  let has_stake address t = Big_map.mem address t

  let join address t = Big_map.add address () t

  let burn address t = Big_map.remove address t
end

(*
  Assume that C committed to something.
  The game goes like this:
  1. R starts the game by rejecting C's commitment, and gives a number of steps.
  2. C answers by giving the number of executions steps.
    The number of steps is agreed and it is the min of both.
    Let's say the number of steps = 1000.
  3. R gives h100, h200, h300, ... h1000
  4. C states the first one they disagree with
  5. Repeat 3&4 until we get a single hash or the last one
  6. R gives the proof from h_n to h_n+1 and replays it
  7. We see who was correct between R and C
*)
module VM : sig
  type t
  val execute_step : t -> t
  val hash : t -> state_hash
end = struct
  type t

  let execute_step _ = assert false
  let hash _ = assert false
end

type player = Rejector | Committer
module Rejection_game
    (* : sig
         type player = Rejector | Committer

         type state

         type vote = Agree | Disagree
         type action =
           | Send_hash of state_hash
           (* TODO: vote *)
           | Vote of vote
           | Replay of VM.t

         val start :
           previous_state_hash:state_hash ->
           committer_state_hash:state_hash ->
           committer_steps:step ->
           rejector_state_hash:state_hash ->
           rejector_steps:step ->
           rejector_mid_state_hash:state_hash ->
           state
         val play : player -> action -> state -> state
         val has_winner : state -> player option
       end *) =
struct
  (* TODO: if we require commit to include steps we can skip a couple turns *)
  (* TODO: log2 of steps to know maximum of turns instead of holding each individual step, thank you Daniel*)

  type player = Rejector | Committer
  type state =
    | Vote_on_midpoint of {
        initial : state_hash * step;
        mid : state_hash * step;
        final : state_hash * step * player;
      }
    | Waiting_midpoint of {
        initial : state_hash * step;
        final : state_hash * step * player;
      }
    | To_replay of { state_hash : state_hash; expected : state_hash * player }
    | Winner of player

  type game_id = nat
  type nonrec game =
    | Root of state
    | Child of {
        weight : level;
        parent : game_id;
        parent_player : player;
        state : state;
      }

  (* TODO: a game can only have a single fork per level *)
  let find_game : game_id -> game = assert false

  type vote = Agree | Disagree
  type action =
    | Send_hash of state_hash
    (* TODO: vote *)
    | Vote of vote
    | Replay of VM.t

  let calculate_mid_step ~initial_step ~final_step =
    let diff = abs (final_step - initial_step) in
    match diff / 2n with
    | Some (mid_step, _remainder) -> mid_step
    | None -> assert false

  let start ~previous_state_hash ~committer_state_hash ~committer_steps
      ~rejector_state_hash ~rejector_steps ~rejector_mid_state_hash =
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
      let mid_step = calculate_mid_step ~initial_step ~final_step in
      (rejector_mid_state_hash, mid_step)
    in
    Vote_on_midpoint { initial; mid; final }

  (* TODO: proof showing that committer always agrees with initial hash *)
  (* TODO: proof showing that committer almost always disagrees with final hash *)

  let get_weight game =
    match game with
    | Root _ -> 0n
    | Child { weight; parent = _; parent_player = _; state = _ } -> weight

  let get_state game =
    match game with
    | Root state -> state
    | Child { weight = _; parent = _; parent_player = _; state } -> state

  let has_winner game =
    match get_state game with Winner player -> Some player | _ -> None

  (* empty block has any steps? If so initial will never be equal to final *)
  let play player action game =
    let state = get_state game in
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
        if initial_step + 1n = final_step then
          To_replay
            { state_hash = initial_hash; expected = (final_hash, expecting) }
        else Waiting_midpoint { initial; final }
    | To_replay { state_hash; expected }, Rejector, Replay state ->
        (* TODO: analyze this,
            if it fails it will reject and the rejector will timeout eventually *)
        (* TODO: slash instead of fail *)
        let () = assert (VM.hash state = state_hash) in
        let state' = VM.execute_step state in

        let expected_hash, expected_author = expected in
        let opposite_player = function
          | Rejector -> Committer
          | Committer -> Rejector
        in
        let winner =
          if VM.hash state' = expected_hash then expected_author
          else opposite_player expected_author
        in

        (* a fork only has effect if:

           G is the game that was forked
           G' is the new game based on G

           A is the player who's game turn was forked
           B is the player who forked G as A

           If A will lose G
           AND B wins G'

           The last person to fork and win, is always the honest validator
        *)

        (*
you only fork if both sides are wrong
*)
        let () =
          match game with
          | Root _ -> ()
          | Child { weight = _; parent; parent_player = _; state = _ } -> (
              let parent = find_game parent in

              match has_winner parent with
              | Some _ -> ()
              | None -> (* TODO: slash instead of fail *) assert false)
        in

        Winner winner
    | _ ->
        (* TODO: slash instead of fail *)
        assert false
end

type commit_rejection_game = {
  state : Rejection_game.state;
  rejector : address;
}

module Rejection_lazy_map : sig
  type game_id

  type t

  val empty : unit -> t

  (* O(log n) *)
  val append : rejection_game -> t -> t option

  val fork : unit

  (* O(log n) *)
  val find_opt : t -> rejection_game option
end = struct
  type t = { length : nat; items : (address, Rejection_game.state) big_map }

  let empty () = { length = 0n; items = Big_map.empty }

  let append game t =
    if Big_map.mem sender t.items then None
    else
      let length = t.length + 1n in
      let items = Big_map.add sender game t.items in
      Some { length; items }

  let find_opt t = Big_map.find_opt sender t.items
end

(* THE IMPORTANT THING IS WE'RE BURNING SOMEONE'S MONEY *)
type commit_data = {
  committer : address;
  (* TODO: validate steps is inside of the commit_hash *)
  steps : step;
  rejections : Rejection_lazy_map.t;
}

module Commit_lazy_map : sig
  type t

  val empty : unit -> t

  (* O(log n) *)
  (* [append state_hash] None when duplicated *)
  val append : state_hash -> t -> t option

  (* O(log n) *)
  val find_opt : state_hash -> t -> commit_data option

  (* O(log n) *)
  val mem : state_hash -> t -> bool

  (* O(1) *)
  val length : t -> nat
end = struct
  type t = { length : nat; items : (state_hash, commit) big_map }
  let empty () = { length = 0n; items = Big_map.empty }
  let append state_hash t =
    if Big_map.mem state_hash t.items then None
    else
      let commit = { sender; rejections = Rejection_lazy_map.empty () } in
      let length = t.length + 1n in
      let items = Big_map.add state_hash commit t.items in
      Some { length; items }

  let find_opt state_hash t = Big_map.find_opt state_hash t.items
  let mem state_hash t = Big_map.mem state_hash t.items

  let length t = t.length
end

module Submission_lazy_map : sig
  type t

  val empty : unit -> t

  (* O(log n) *)
  val append : submission -> t -> t

  (* O(log n) *)
  val find_opt : nat -> t -> submission option

  (* O(1) *)
  val length : t -> nat
end = struct
  type t = { length : nat; items : (nat, submission) big_map }
  let empty () = { length = 0n; items = Big_map.empty }
  let append submission t =
    let length = t.length + 1n in
    let items = Big_map.add t.length submission t.items in
    { length; items }

  let find_opt index t = Big_map.find_opt index t.items
  let length t = t.length
end

module Committers_lazy_set : sig
  type t
  val empty : unit -> t
  val append : address -> t -> t option
  val mem : address -> t -> bool
end = struct
  type t = (address, unit) big_map
  let empty () = Big_map.empty
  let mem address t = Big_map.mem address t
  let append address t =
    if mem address t then None
    else
      let t = Big_map.add address () t in
      Some t
end

(* TODO: abstract this *)
module Level_data = struct
  type t = {
    submissions : Submission_lazy_map.t;
    commits : Commit_lazy_map.t;
    committers : Committers_lazy_set.t;
  }
  let empty () =
    {
      submissions = Submission_lazy_map.empty ();
      commits = Commit_lazy_map.empty ();
      committers = Committers_lazy_set.empty ();
    }
end

module Black_list_lazy_map = struct
  type t
end
type storage = {
  (* TODO: alive : bool; *)
  levels : (level, Level_data.t) big_map;
  trusted : state_hash * level;
  collateral_vault : Collateral_vault.t;
      (* TODO: do we need trusted_state_hash for something?*)
      (* trusted_state_hash : state_hash; *)
      (* TODO: use unique identity instead of blacklist *)
}

(* O(log2 levels) + O(log2 submissions) *)
let submit (submission : submission) (storage : storage) =
  let { levels; trusted; collateral_vault } = storage in
  let Level_data.{ submissions; commits; committers } =
    match Big_map.find_opt current_level levels with
    | Some level_data -> level_data
    | None -> Level_data.empty ()
  in
  let submissions = Submission_lazy_map.append submission submissions in
  let level_data = Level_data.{ submissions; commits; committers } in
  let levels = Big_map.add current_level level_data levels in
  { levels; trusted; collateral_vault }

(* O(log2 collateral_vault) *)
let join (storage : storage) =
  let { levels; trusted; collateral_vault } = storage in
  let () = assert (Tezos.amount >= stake_amount) in
  let () = assert (not (Collateral_vault.has_stake sender collateral_vault)) in
  let collateral_vault = Collateral_vault.join sender collateral_vault in
  { levels; trusted; collateral_vault }

(* O(log2 collateral_vault) + transaction *)
let exit (storage : storage) =
  let { levels; trusted; collateral_vault } = storage in
  let () = assert (Collateral_vault.has_stake sender collateral_vault) in
  let collateral_vault = Collateral_vault.burn sender collateral_vault in

  let contract =
    match Tezos.get_contract_opt sender with
    | Some contract -> contract
    | None -> failwith "failed to send your money back"
  in
  let transaction = Tezos.transaction () stake_amount contract in
  ([ transaction ], { levels; trusted; collateral_vault })

let round_time = 10n

let time_to_respond = 1n * round_time

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
let is_finalized_level : level -> storage -> bool = assert false

let commit (level : level) (new_state_hash : state_hash) (steps : step)
    (storage : storage) =
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake sender collateral_vault) in
  let () = assert (is_open_level level) in

  let Level_data.{ submissions; commits; committers } =
    match Big_map.find_opt level levels with
    | Some level_data -> level_data
    | None -> Level_data.empty ()
  in

  let committers =
    match Committers_lazy_set.append sender committers with
    | Some committers -> committers
    | None -> failwith "duplicated committer"
  in
  let commits =
    match Commit_lazy_map.append new_state_hash commits with
    | Some commits -> commits
    | None -> failwith "duplicated commit"
  in

  let level_data = Level_data.{ submissions; commits; committers } in
  let levels = Big_map.add level level_data levels in
  { levels; trusted; collateral_vault }

let start_rejection_game (new_rejection_game : new_rejection_game)
    (storage : storage) =
  let {
    level;
    committer_state_hash;
    rejector_mid_state_hash;
    rejector_state_hash;
    rejector_steps;
  } =
    new_rejection_game
  in
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake sender collateral_vault) in
  let () = assert (not (is_finalized_level level storage)) in

  let Level_data.{ submissions = _; commits; committers = _ } =
    match Big_map.find_opt level levels with
    | Some level_data -> level_data
    | None -> Level_data.empty ()
  in

  let { sender = _; steps = committer_steps; rejections } =
    match Commit_lazy_map.find_opt committer_state_hash commits with
    | Some commit_data -> commit_data
    | None -> failwith "invalid committer state hash"
  in

  let trusted_state_hash, _trusted_level = trusted in
  let rejection_game =
    Rejection_game.start ~previous_state_hash:trusted_state_hash
      ~committer_state_hash ~committer_steps ~rejector_state_hash
      ~rejector_steps ~rejector_mid_state_hash
  in
  ()

(* let trust_commit (level : level) (state_hash : state_hash) (storage : storage) =
   let { levels; trusted_level; collateral_vault } = storage in

   let () = assert (level = trusted_level + 1n) in
   let () = assert (is_finalized_level level storage) in

   let Level_data.{ submissions = _; commits; committers = _ } =
     (* TODO: duplicated access to level_data, inside of is_finalized_level *)
     match Big_map.find_opt level levels with
     | Some level_data -> level_data
     | None -> Level_data.empty ()
   in

   let () = assert (Commit_lazy_map.length commits = 1n) in
   let _commit =
     match Commit_lazy_map.find_opt state_hash commits with
     | Some commit -> commit
     | None -> failwith "bullshit state hash"
   in

   (* TODO: think it again, about removing rejections or not *)
   (* let () = assert (Rejection_lazy_map.length commit.rejections = 1n) in *)
   let levels = Big_map.remove level levels in
   { levels; trusted_level = level; collateral_vault } *)

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
  | Commit (level, state_hash, steps) ->
      let storage = commit level state_hash steps storage in
      (([] : operation list), storage)
  (* | Trust_commit (state_hash, level) ->
      let storage = trust_commit state_hash level storage in
      (([] : operation list), storage) *)
  | _ -> assert false
