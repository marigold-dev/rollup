open Environment

(* THE IMPORTANT THING IS WE'RE BURNING SOMEONE'S MONEY *)

(* IMPORTANT: the honest validator will never loose at anything *)
(* anyone can defend a commit *)

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

(* TODO: split in two state machines, rejection game and optimistic rollup *)

let stake_amount : mutez = assert false
let commitment_amount : tez = (* 1000tz *) assert false

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
  5. Repeat 3&4 until we get a single hash or the last one
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

type new_rejection_game

type parameter =
  (* users *)
  | Submit of submission
  (* validators *)
  | Join
  | Exit
  | Commit of { level : level; state_hash : state_hash; steps : steps }
  | Reject of {
      level : level;
      committer : committer;
      (* TODO: ensure that a committer is always using the same defend_as *)
      defend_as : committer;
      mid_state_hash : state_hash;
    }
  | Fork_commit of { committer : committer }
  (* | Trust_commit of level * state_hash *)
  (* TODO? fuse commit *)
  | Start_rejection_game of new_rejection_game
  | Send_middle_hash of { committer : address; state_hash : state_hash }
  | Vote_on_middle of { rejector : address; vote : unit }
  | Replay of { committer : address; state : Vm.t }

(* O(1) *)
let submit (_ : submission) (storage : storage) = storage

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

let append_commit ~level commit committers storage =
  let committer = sender in
  let { levels; trusted; collateral_vault } = storage in
  let committers =
    match Committer_lazy_map.append committer commit committers with
    | Some committers -> committers
    | None -> failwith "duplicated committer"
  in
  let levels = Big_map.add level committers levels in
  { levels; trusted; collateral_vault }

let commit level new_state_hash steps storage =
  let { levels; trusted = _; collateral_vault } = storage in
  let () = assert (Collateral_vault.has_stake sender collateral_vault) in
  let () = assert (is_open_level level) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    | None -> Committer_lazy_map.empty ()
  in

  let commit = Commit.make new_state_hash steps in
  append_commit ~level commit committers storage

(* TODO: when can you fork? *)
let fork_commit ~level ~base_committer storage =
  let { levels; trusted = _; collateral_vault } = storage in
  let () = assert (Collateral_vault.has_stake sender collateral_vault) in
  let () = assert (not (is_finalized_level level storage)) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    | None -> Committer_lazy_map.empty ()
  in

  let base_commit = assert false in
  let commit =
    match Commit.fork base_commit with
    | Some commit -> commit
    | None -> failwith "a recently created commit cannot be forked"
  in
  append_commit ~level commit committers storage

let append_game () = ()
let reject ~level ~committer ~defend_as ~mid_state_hash ~storage =
  (* TODO: prevent duplicated rejection game, if it matters *)
  let new_rejector = sender in
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake new_rejector collateral_vault) in
  (* TODO: not finalized is not the right way to say it *)
  let () = assert (not (is_finalized_level level storage)) in

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
    | None -> failwith "invalid committer state hash"
  in

  let rejector_commit =
    match Committer_lazy_map.find defend_as committers with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid rejector committer"
  in

  let trusted_state_hash, _trusted_level = trusted in
  let game = assert false in

  let committer_commit =
    match Commit.append_game ~rejector:new_rejector game committer_commit with
    | Some commit -> commit
    | None -> failwith "duplicated rejectin game"
  in

  (* TODO: abstract this in Committer_lazy_map *)
  let committers =
    match Committer_lazy_map.update committer committer_commit committers with
    | Some committer -> committer
    | None -> failwith "unreachablçe"
  in
  let levels = Big_map.add level committers levels in

  { levels; trusted; collateral_vault }
let fork_reject ~level ~committer ~base_rejector storage =
  let { levels; trusted; collateral_vault } = storage in
  let () = assert (Collateral_vault.has_stake base_rejector collateral_vault) in
  (* TODO: not finalized is not the right way to say it *)
  let () = assert (not (is_finalized_level level storage)) in

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
    | None -> failwith "invalid committer state hash"
  in

  let base_game =
    match Commit.find_game ~rejector:base_rejector commit with
    | Some commit -> commit
    (* TODO: should this be a slash??? *)
    | None -> failwith "invalid rejector committer"
  in

  let trusted_state_hash, _trusted_level = trusted in
  let game = assert false in
  assert false

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
  | Reject { level; committer; defend_as; mid_state_hash } ->
      let storage =
        reject ~level ~committer ~defend_as ~mid_state_hash ~storage
      in
      assert false
  | _ -> assert false

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
  | Reject { level; committer; _ } ->
    assert false
  (* | Trust_commit (state_hash, level) ->
      let storage = trust_commit state_hash level storage in
      (([] : operation list), storage) *)
  | _ -> assert false
