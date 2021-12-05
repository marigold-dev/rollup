open Environment

(* THE IMPORTANT THING IS WE'RE BURNING SOMEONE'S MONEY *)
(*
Our game, is the simple game, but lazy


*)

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
(* TODO? fuse commit *)

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

let commit ~level ~parent_state_hash ~state_hash ~steps storage =
  let { levels; trusted = _; collateral_vault } = storage in
  let () = assert (Collateral_vault.has_stake sender collateral_vault) in
  (* TODO: magic number *)
  let () = assert (Turn.current ~level = 0n) in

  let committers =
    match Big_map.find_opt level levels with
    | Some committers -> committers
    | None -> Committer_lazy_map.empty ()
  in

  let commit = Commit.make ~level ~parent_state_hash ~state_hash ~steps in
  append_commit ~level commit committers storage

(* TODO: when can you fork? *)
let fork_commit ~level ~base_committer storage =
  let { levels; trusted = _; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake sender collateral_vault) in

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
  append_commit ~level commit committers storage

let reject ~level ~committer ~defend_as ~mid_state_hash ~storage =
  (* TODO: prevent duplicated rejection game, if it matters *)
  let new_rejector = sender in
  let { levels; trusted; collateral_vault } = storage in

  let () = assert (Collateral_vault.has_stake new_rejector collateral_vault) in
  (* TODO: magic number *)
  let () = assert (Turn.current ~level = 2n) in

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
    Rejection_game.start ~level
      ~previous_state_hash:(Commit.parent_state_hash committer_commit)
      ~committer:
        (Commit.state_hash committer_commit, Commit.steps committer_commit)
      ~rejector:(Commit.state_hash rejector_commit, Commit.steps rejector_commit)
      ~mid_state_hash
  in

  let committer_commit =
    match Commit.append_game ~rejector:new_rejector game committer_commit with
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

let fork_reject ~level ~committer ~base_rejector storage =
  let rejector = sender in
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
    | None -> failwith "invalid committer state hash"
  in

  let base_game =
    match Commit.find_game ~rejector:base_rejector commit with
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
    match Commit.append_game ~rejector game commit with
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

let handle_move_result ~committer (move_result : Commit.move_result option)
    committers =
  match move_result with
  | Some Rejector_won -> Committer_lazy_map.remove committer committers
  | Some (Committer_won commit) | Some (Commit commit) ->
      Committer_lazy_map.update committer commit committers
  | None -> failwith "invalid rejector"
let slash_move_result ~committer ~rejector
    (move_result : Commit.move_result option) collateral_vault =
  match move_result with
  | Some (Committer_won _) -> Collateral_vault.burn rejector collateral_vault
  | Some Rejector_won -> Collateral_vault.burn committer collateral_vault
  | Some (Commit _) | None -> collateral_vault

let defend ~level ~rejector ~move storage =
  let committer = sender in
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
    | None -> failwith "invalid committer state hash"
  in

  let move_result = Commit.defend ~rejector move commit in
  let collateral_vault =
    slash_move_result ~committer ~rejector move_result collateral_vault
  in
  let committers =
    match handle_move_result ~committer move_result committers with
    | Some committers -> committers
    | None -> assert false
  in

  let levels = Big_map.add level committers levels in

  { levels; trusted; collateral_vault }

let attack ~level ~committer ~move storage =
  let rejector = sender in
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
    | None -> failwith "invalid committer state hash"
  in
  let move_result = Commit.attack ~rejector move commit in
  let collateral_vault =
    slash_move_result ~committer ~rejector move_result collateral_vault
  in
  let committers =
    match handle_move_result ~committer move_result committers with
    | Some committers -> committers
    | None -> assert false
  in
  let levels = Big_map.add level committers levels in

  { levels; trusted; collateral_vault }

let trust_commit ~level ~committer storage =
  let { levels; trusted; collateral_vault } = storage in

  (* TODO: is this needed *)
  let () = assert (Collateral_vault.has_stake sender collateral_vault) in

  let trusted_state_hash, trusted_level = trusted in
  let () = assert (level = trusted_level + 1n) in
  let () = assert (Turn.current ~level >= 3n) in

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
      let storage =
        commit ~level ~parent_state_hash ~state_hash ~steps storage
      in
      (([] : operation list), storage)
  | Fork_commit { level; committer } ->
      let storage = fork_commit ~level ~base_committer:committer storage in
      (([] : operation list), storage)
  | Reject { level; committer; defend_as; mid_state_hash } ->
      let storage =
        reject ~level ~committer ~defend_as ~mid_state_hash ~storage
      in
      (([] : operation list), storage)
  | Fork_reject { level; committer; rejector } ->
      let storage =
        fork_reject ~level ~committer ~base_rejector:rejector storage
      in
      (([] : operation list), storage)
  | Defend { level; rejector; move } ->
      let storage = defend ~level ~rejector ~move storage in
      (([] : operation list), storage)
  | Attack { level; committer; move } ->
      let storage = attack ~level ~committer ~move storage in
      (([] : operation list), storage)
  | Trust_commit { level; committer } ->
      let storage = trust_commit ~level ~committer storage in
      (([] : operation list), storage)

(* because the honest player is always playing the simple game,
   where they can defend their commit against everyone else and
   attack everyone else

   a dishonest player literally cannot do anything to change the outcome of a game on the game level
*)
