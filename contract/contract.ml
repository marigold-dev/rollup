open Tezos_environment

(* THE IMPORTANT THING IS WE'RE BURNING SOMEONE'S MONEY *)

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

(* TODO: BIG TODO: go over all asserts and slash whoever is needed *)

(* TODO: put all required money to be a honest validator on the contract before starting rejections or commits *)

(* TODO: commits are allowed to also clean a level to avoid paying for increasing the storage *)

(* TODO: batch parameter to be more efficient in gas*)
type committer = address
type rejector = address
type new_rejection_game = {
  level : level;
  (* TODO: this needs to be a known state_hash on this level *)
  committer : committer;
  rejector_mid_state_hash : state_hash;
  rejector_state_hash : state_hash;
  rejector_steps : step;
}

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
let round_time = 10n

let time_to_respond = 1n * round_time
let time_to_timeout = 2n * round_time
let cooldown_period = 1n * round_time

module Small_rejection_game : sig
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
  val expected_player : state -> player option
  val has_winner : state -> player option
end = struct
  (* TODO: if we require commit to include steps we can skip a couple turns *)
  (* TODO: if we require the committer to send the hash, we can fuse turns *)
  (* TODO: remove committer if it looses as a rejector *)
  (* TODO: log2 of steps to know maximum of turns instead of holding each individual step, thank you Daniel*)

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

  type vote = Agree | Disagree
  type action = Send_hash of state_hash | Vote of vote | Replay of VM.t

  (* TODO: a game can only have a single fork per level *)
  (* let find_game : game_id -> game = assert false *)

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

  let has_winner state =
    match state with Winner player -> Some player | _ -> None

  (* empty block has any steps? If so initial will never be equal to final *)
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
        if initial_step + 1n = final_step then
          To_replay
            { state_hash = initial_hash; expected = (final_hash, expecting) }
        else Waiting_midpoint { initial; final }
    | To_replay { state_hash; expected }, Rejector, Replay state ->
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

        Winner winner
    | _ ->
        (* TODO: slash instead of fail *)
        assert false

  let expected_player state =
    match state with
    | Waiting_midpoint _ -> Some Rejector
    | Vote_on_midpoint _ -> Some Committer
    | To_replay _ -> Some Rejector
    | Winner _ -> None
end

module Large_rejection_game : sig
  open Small_rejection_game

  type state
  val has_winner : state -> player option

  val start :
    previous_state_hash:state_hash ->
    committer_state_hash:state_hash ->
    committer_steps:step ->
    rejector_state_hash:state_hash ->
    rejector_steps:step ->
    rejector_mid_state_hash:state_hash ->
    state

  val play : player -> action -> state -> state
  val timeout : player -> state -> state
  val fork : player -> action -> state -> state
end = struct
  open Small_rejection_game
  type nonrec state = {
    previous_state : state;
    previous_action : action;
    (* previous_action_level : level; *)
    current_state : state;
  }

  type nonrec action = Play of action | Claim_timeout | Fork of action

  let assert_is_expected_player player state =
    (* TODO: slash instead of fail *)
    match expected_player state.current_state with
    | None -> failwith "game already finished"
    | Some expected_player when player = expected_player -> ()
    | Some _ -> failwith "not your turn"

  let play player action state =
    (* TODO: maybe derive player from the action itself??? *)
    (* let () = state in *)
    let () = assert_is_expected_player player state in
    (* let () = assert (not (is_cooldown )) *)
    (* let x = 1 in *)
    state

  let timeout = assert false
  let fork player action state =
    let { previous_state; previous_action; current_state = _ } = state in
    let () =
      (* TODO: slash instead of fail *)
      match (state.previous_action, action) with
      | Send_hash previous_hash, Send_hash alternative_hash
        when previous_hash <> alternative_hash ->
          ()
      | Vote Disagree, Vote Agree -> ()
      | Vote Agree, Vote Disagree -> ()
      | Replay _, Replay _ ->
          (* TODO: maybe if we slash this can be the case??? *)
          failwith "cannot fork a replay, same result"
      | _ -> failwith "invalid movement for this fork"
    in
    let x = play in
    state
end

module Rejection_lazy_map : sig
  type t

  val empty : unit -> t

  (* O(log n) *)
  val append : rejector:rejector -> Large_rejection_game.state -> t -> t option

  (* O(log n) *)
  val remove : rejector:rejector -> t -> t option

  (* O(log n) *)
  val find_opt : rejector:rejector -> t -> Large_rejection_game.state option
end = struct
  type t = {
    length : nat;
    items : (rejector, Large_rejection_game.state) big_map;
  }

  let empty () = { length = 0n; items = Big_map.empty }

  let append ~rejector game t =
    if Big_map.mem rejector t.items then None
    else
      let length = t.length + 1n in
      let items = Big_map.add rejector game t.items in
      Some { length; items }

  let remove ~rejector t =
    (* TODO: assert properties required to remove a rejection game *)
    if Big_map.mem rejector t.items then
      let length = abs (t.length - 1n) in
      let items = Big_map.remove rejector t.items in
      Some { length; items }
    else None

  let find_opt ~rejector t = Big_map.find_opt rejector t.items
end

module Commit : sig
  type t

  val make : state_hash -> step -> t

  val state_hash : t -> state_hash
  val steps : t -> step

  (* O(log n) *)
  (* [append_rejection ~rejector rejection commit] None when duplicated *)
  val append_rejection :
    rejector:address -> Large_rejection_game.state -> t -> t option
end = struct
  type t = {
    state_hash : state_hash;
    (* TODO: validate steps is inside of the commit_hash *)
    steps : step;
    rejections : Rejection_lazy_map.t;
  }

  let make state_hash steps =
    { state_hash; steps; rejections = Rejection_lazy_map.empty () }

  let state_hash t = t.state_hash
  let steps t = t.steps
  let rejections t = t.steps

  let append_rejection ~rejector rejection t =
    let { state_hash; steps; rejections } = t in
    match Rejection_lazy_map.append ~rejector rejection rejections with
    | Some rejections -> Some { state_hash; steps; rejections }
    | None -> None
end

module Committer_lazy_map : sig
  type t

  val empty : unit -> t

  (* O(log n) *)
  (* [append state_hash] None when duplicated *)
  val append : committer -> Commit.t -> t -> t option

  (* O(log n) *)
  (* [update committer commit t] None when missing *)
  val update : committer -> Commit.t -> t -> t option

  (* O(log n) *)
  (* [remove committer t] None when missing *)
  val remove : committer -> t -> t option

  (* O(log n) *)
  val fork : from:committer -> committer -> t -> t

  (* O(log n) *)
  val find_opt : committer -> t -> Commit.t option

  (* O(log n) *)
  val mem : committer -> t -> bool

  (* O(1) *)
  (* TODO: why do we need the length???*)
  val length : t -> nat
end = struct
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

(* TODO: abstract this *)
module Level_data = struct
  type t = {
    submissions : Submission_lazy_map.t;
    committers : Committer_lazy_map.t;
  }
  let empty () =
    {
      submissions = Submission_lazy_map.empty ();
      committers = Committer_lazy_map.empty ();
    }
end

module Black_list_lazy_map = struct
  type t
end

type storage = {
  levels : (level, Level_data.t) big_map;
  trusted : state_hash * level;
  collateral_vault : Collateral_vault.t; (* TODO: alive : bool; *)
}
type parameter =
  (* users *)
  | Submit of submission
  (* validators *)
  | Join
  | Exit
  | Commit of { level : level; state_hash : state_hash; steps : step }
  | Reject of {
      level : level;
      committer : committer;
      rejector_mid_state_hash : state_hash;
      rejector_state_hash : state_hash;
      rejector_steps : step;
    }
  | Fork_commit of { committer : committer }
  | Fork_game of { committer : address; rejector : address }
  (* | Trust_commit of level * state_hash *)
  (* TODO? fuse commit *)
  | Start_rejection_game of new_rejection_game
  | Send_middle_hash of { committer : address; state_hash : state_hash }
  | Vote_on_middle of { rejector : address; vote : Small_rejection_game.vote }
  | Replay of { committer : address; state : VM.t }

(* O(log2 levels) + O(log2 submissions) *)
let submit (submission : submission) (storage : storage) =
  let { levels; trusted; collateral_vault } = storage in
  let Level_data.{ submissions; committers } =
    match Big_map.find_opt current_level levels with
    | Some level_data -> level_data
    | None -> Level_data.empty ()
  in
  let submissions = Submission_lazy_map.append submission submissions in
  let level_data = Level_data.{ submissions; committers } in
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

  let Level_data.{ submissions; committers } =
    match Big_map.find_opt level levels with
    | Some level_data -> level_data
    | None -> Level_data.empty ()
  in

  let commit = Commit.make new_state_hash steps in
  let committers =
    match Committer_lazy_map.append sender commit committers with
    | Some committers -> committers
    | None -> failwith "duplicated committer"
  in

  let level_data = Level_data.{ submissions; committers } in
  let levels = Big_map.add level level_data levels in
  { levels; trusted; collateral_vault }

let reject ~level ~committer ~rejector_mid_state_hash ~rejector_state_hash
    ~rejector_steps ~storage =
  (* TODO: prevent duplicated rejection game, if it matters *)
  let rejector = sender in
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake rejector collateral_vault) in
  (* TODO: not finalized is not the right way to say it *)
  let () = assert (not (is_finalized_level level storage)) in

  let Level_data.{ submissions; committers } =
    match Big_map.find_opt level levels with
    | Some level_data -> level_data
    | None -> Level_data.empty ()
  in

  let commit =
    match Committer_lazy_map.find_opt committer committers with
    | Some commit -> commit
    | None ->
        (* TODO: should this be a slash??? *)
        failwith "invalid committer state hash"
  in
  let committer_state_hash = Commit.state_hash commit in
  let committer_steps = Commit.steps commit in

  let trusted_state_hash, _trusted_level = trusted in
  let game =
    Large_rejection_game.start ~previous_state_hash:trusted_state_hash
      ~committer_state_hash ~committer_steps ~rejector_state_hash
      ~rejector_steps ~rejector_mid_state_hash
  in

  let commit =
    match Commit.append_rejection ~rejector game commit with
    | Some commit -> commit
    | None -> failwith "duplicated rejectin game"
  in

  (* TODO: abstract this in Committer_lazy_map *)
  let committers =
    match Committer_lazy_map.update committer commit committers with
    | Some committer -> committer
    | None -> failwith "unreachablçe"
  in
  let levels =
    Big_map.add level Level_data.{ submissions; committers } levels
  in

  { levels; trusted; collateral_vault }

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
  | Commit { level; state_hash; steps } ->
      let storage = commit level state_hash steps storage in
      (([] : operation list), storage)
  | Reject
      {
        level;
        committer;
        rejector_mid_state_hash;
        rejector_state_hash;
        rejector_steps;
      } ->
      let storage =
        reject ~level ~committer ~rejector_mid_state_hash ~rejector_state_hash
          ~rejector_steps ~storage
      in
      (([] : operation list), storage)
  (* | Trust_commit (state_hash, level) ->
      let storage = trust_commit state_hash level storage in
      (([] : operation list), storage) *)
  | _ -> assert false
