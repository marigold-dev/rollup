open Helpers
open Crypto
open Tezos
module Rejection_game = struct
  type t = {
    level : Z.t;
    (* TODO: this needs to be a known state_hash on this level *)
    committer : Address.t;
    rejector_mid_state_hash : BLAKE2B.t;
    rejector_state_hash : BLAKE2B.t;
    rejector_steps : Z.t;
  }
end

module Vote = struct
  type t = Agree | Disagree
end

module VM = struct
  type t = bytes (* ???? *)
end

type t =
  | Submit of Bytes.t (* done *)
  | Commit of { level : Z.t; state_hash : BLAKE2B.t; step : Z.t } (* done *)
  | Join (* done*)
  | Exit (*done *)
  | Fork_game of { commiter : Address.t; rejector : Address.t } (* done *)
  | Reject of {
      level : Z.t;
      committer : Address.t;
      (* TODO: ensure that a committer is always using the same defend_as *)
      defend_as : Address.t;
      mid_state_hash : BLAKE2B.t;
    } (*done *)
  | Fork_commit of { committer : Address.t } (* done *)
  (* TODO? fuse commit *)
  | Start_rejection_game of Rejection_game.t (* done *)
  | Send_middle_hash of { committer : Address.t; state_hash : BLAKE2B.t } (* done *)
  | Vote_on_middle of { rejector : Address.t; vote : Vote.t } (* done *)
  | Replay of { committer : BLAKE2B.t; state : VM.t }
(* @TODO: Vm.t ??? *)

let of_micheline entrypoint micheline =
  let open Pack in
  let open Tezos_micheline in
  let open Option.Syntax in
  match (entrypoint, micheline) with
  | ( "commit",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [
            Prim (_, D_Pair, [ Int (_, level); Bytes (_, state_hash) ], _);
            Int (_, step);
          ],
          _ ) ) ->
      (* @TODO: properly handle this *)
      let* state_hash = state_hash |> Bytes.to_string |> BLAKE2B.of_string in
      Some (Commit { level; state_hash; step })
  | "submit", Micheline.Bytes (_, submission) -> Some (Submit submission)
  | ( "fork_game",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [ Micheline.String (_, commiter); String (_, rejector) ],
          _ ) ) ->
      let open Tezos.Address in
      let* commiter = of_string commiter in
      let* rejector = of_string rejector in
      Some (Fork_game { commiter; rejector })
  | "join", _ -> Some Join
  | "exit", _ -> Some Exit
  | ( "reject",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [
            Prim (_, D_Pair, [ Int (_, level); String (_, committer) ], _);
            Prim
              ( _,
                D_Pair,
                [ String (_, defend_as); Bytes (_, mid_state_hash) ],
                _ );
          ],
          _ ) ) ->
      let open Tezos.Address in
      let* committer = of_string committer in
      let* defend_as = of_string defend_as in
      let* mid_state_hash =
        BLAKE2B.of_string (Bytes.to_string mid_state_hash)
      in
      Some (Reject { level; committer; defend_as; mid_state_hash })
  | "fork_commit ", Micheline.String (_, committer) ->
      let* committer = Address.of_string committer in
      Some (Fork_commit { committer })
  | ( "vote_on_middle",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [ Micheline.String (_, rejector); String (_, vote) ],
          _ ) ) ->
      let vote =
        match vote with
        | "agree" -> Vote.Agree
        | "disagree" -> Disagree
        | _ -> assert false
      in
      let* rejector = Address.of_string rejector in
      Some (Vote_on_middle { vote; rejector })
  | ( "send_middle_hash",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [ Micheline.String (_, committer); Bytes (_, state_hash) ],
          _ ) ) ->
      let* committer = Address.of_string committer in
      let* state_hash = BLAKE2B.of_string (Bytes.to_string state_hash) in
      Some (Send_middle_hash { committer; state_hash })
  | ( "start_rejection_game",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [
            Prim
              ( _,
                D_Pair,
                [
                  Prim (_, D_Pair, [ Int (_, level); String (_, committer) ], _);
                  Prim
                    ( _,
                      D_Pair,
                      [
                        Bytes (_, rejector_mid_state_hash);
                        Bytes (_, rejector_state_hash);
                      ],
                      _ );
                ],
                _ );
            Int (_, rejector_steps);
          ],
          _ ) ) ->
      let* committer = Address.of_string committer in
      let* rejector_mid_state_hash =
        BLAKE2B.of_string (Bytes.to_string rejector_mid_state_hash)
      in
      let* rejector_state_hash =
        BLAKE2B.of_string (Bytes.to_string rejector_state_hash)
      in
      Some
        (Start_rejection_game
           {
             committer;
             rejector_steps;
             rejector_mid_state_hash;
             rejector_state_hash;
             level;
           })
  | _ -> None
