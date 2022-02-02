open Environment
open Entry

type parameter =
  (* users *)
  | Submit       of submission
  (* validators *)
  | Join
  | Quit
  | Commit       of {
      level : level;
      previous_state_hash : state_hash;
      state_hash : state_hash;
      steps : steps;
    }
  | Reject       of {
      level : level;
      committer : committer;
      (* TODO: ensure that a rejector is always using the same defend_as hash *)
      defend_as : committer;
    }
  | Defend       of {
      level : level;
      rejector : rejector;
      move : Temporal_rejection_game.move;
    }
  | Attack       of {
      level : level;
      committer : committer;
      move : Temporal_rejection_game.move;
    }
  | Trust_commit of {
      level : level;
      committer : committer;
    }

(* TODO: important gas optimization,
       fusing many defends and attacks to avoid refetching data *)

module Main (Tezos : Tezos) = struct
  module Submit = Submit (Tezos)
  module Join = Join (Tezos)
  module Quit = Quit (Tezos)
  module Commit = Commit (Tezos)
  module Reject = Reject (Tezos)
  module Defend = Defend (Tezos)
  module Attack = Attack (Tezos)
  module Trust_commit = Trust_commit (Tezos)

  let assert_requirements action storage =
    let requires =
      match action with
      | Submit _ -> Submit.requires
      | Join -> Join.requires
      | Quit -> Quit.requires
      | Commit _ -> Commit.requires
      | Reject _ -> Reject.requires
      | Defend _ -> Defend.requires
      | Attack _ -> Attack.requires
      | Trust_commit _ -> Trust_commit.requires in

    match requires with
    | Some Tez ->
      (* TODO: should we do >= stake_amount*)
      assert (Tezos.amount = stake_amount)
    | Some Stake -> assert (Storage.has_stake Tezos.sender storage)
    | None -> ()

  let execute action storage =
    let () = assert_requirements action storage in

    match action with
    | Submit submission -> Submit.execute ~submission storage
    | Join -> Join.execute storage
    | Quit -> Quit.execute storage
    | Commit { level; previous_state_hash; state_hash; steps } ->
      Commit.execute ~level ~previous_state_hash ~state_hash ~steps storage
    | Reject { level; committer; defend_as } ->
      Reject.execute ~level ~committer ~defend_as storage
    | Defend { level; rejector; move } ->
      Defend.execute ~level ~rejector ~move storage
    | Attack { level; committer; move } ->
      Attack.execute ~level ~committer ~move storage
    | Trust_commit { level; committer } ->
      Trust_commit.execute ~level ~committer storage
end
