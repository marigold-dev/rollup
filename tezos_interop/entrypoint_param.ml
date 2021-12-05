open Helpers
open Crypto
open Tezos

module Vote = struct
  type t = Agree | Disagree
  let of_micheline =
    let open Pack in
    let open Tezos_micheline in
    let open Michelson_v1_primitives in
    function
    | Micheline.Prim (_, D_Left, [ Prim (_, D_Unit, _, _) ], _) -> Some Agree
    | Micheline.Prim (_, D_Right, [ Prim (_, D_Unit, _, _) ], _) ->
        Some Disagree
    | _ -> None
end
module Vm = struct
  module Pool = struct
    type t = (Z.t * BLAKE2B.t) list
    let traverse_option f l =
      let open Option.Syntax in
      let rec aux f acc l =
        match l with
        | [] -> Option.some (List.rev acc)
        | x :: tail ->
            let* x' = f x in
            aux f (x' :: acc) tail
      in
      aux f [] l
    let of_micheline =
      let open Pack in
      let open Tezos_micheline in
      let open Michelson_v1_primitives in
      let open Option.Syntax in
      fun lst ->
        traverse_option
          (function
            | Micheline.Prim (_, D_Pair, [ Int (_, level); Bytes (_, hash) ], _)
              ->
                let* hash = BLAKE2B.of_bytes hash in
                Some (level, hash)
            | _ -> None)
          lst
  end

  type t = { level : Z.t; steps : Z.t; counter : Z.t; pool : Pool.t }

  let of_micheline =
    let open Pack in
    let open Tezos_micheline in
    let open Michelson_v1_primitives in
    let open Option.Syntax in
    function
    | Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [
            Prim (_, D_Pair, [ Int (_, level); Int (_, steps) ], _);
            Prim (_, D_Pair, Int (_, counter) :: pool, _);
          ],
          _ ) ->
        let* pool = Pool.of_micheline pool in
        Some { level; steps; counter; pool }
    | _ -> None
end
module Rejection_game = struct
  type defend = Vote of Vote.t | Timeout
  type attack = Mid_hash of BLAKE2B.t | Replay of Vm.t | Timeout

  let defend_of_micheline =
    let open Pack in
    let open Tezos_micheline in
    let open Michelson_v1_primitives in
    function
    | Micheline.Prim (_, D_Right, [ prim ], _) ->
        let open Option.Syntax in
        let* vote = Vote.of_micheline prim in
        Some (Vote vote)
    | Micheline.Prim (_, D_Left, [ Prim (_, D_Left, _, _) ], _) -> Some Timeout
    | _ -> None
  let attack_of_micheline =
    let open Pack in
    let open Tezos_micheline in
    let open Michelson_v1_primitives in
    let open Option.Syntax in
    function
    | Micheline.Prim (_, D_Left, [ Prim (_, D_Left, [ Bytes (_, hash) ], _) ], _)
      ->
        let* hash = BLAKE2B.of_bytes hash in
        Some (Mid_hash hash)
    | Micheline.Prim (_, D_Left, [ Prim (_, D_Right, [ prim ], _) ], _) ->
        let* vm = Vm.of_micheline prim in
        Some (Replay vm)
    | Micheline.Prim (_, D_Right, [ Prim (_, D_Unit, _, _) ], _) -> Some Timeout
    | _ -> None
end

type t =
  | Submit of BLAKE2B.t (* done *)
  | Commit of {
      level : Z.t;
      parent_state_hash : BLAKE2B.t;
      state_hash : BLAKE2B.t;
      step : Z.t;
    } (* done *)
  | Join (* done*)
  | Exit (*done *)
  | Reject of {
      level : Z.t;
      committer : Address.t;
      (* TODO: ensure that a committer is always using the same defend_as *)
      defend_as : Address.t;
      mid_state_hash : BLAKE2B.t;
    } (*done *)
  | Fork_reject of { commiter : Address.t; rejector : Address.t } (* done *)
  | Defend of {
      level : Z.t;
      rejector : Address.t;
      move : Rejection_game.defend;
    } (* done *)
  | Attack of {
      level : Z.t;
      committer : Address.t;
      move : Rejection_game.attack;
    }
  | Trust_commit of { level : Z.t; committer : Address.t }

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
            Prim (_, D_Pair, [ Int (_, level); Bytes (_, parent_state_hash) ], _);
            Prim (_, D_Pair, [ Bytes (_, state_hash); Int (_, step) ], _);
          ],
          _ ) ) ->
      (* @TODO: properly handle this *)
      let* state_hash = BLAKE2B.of_bytes state_hash
      and* parent_state_hash = BLAKE2B.of_bytes parent_state_hash in
      Some (Commit { level; state_hash; parent_state_hash; step })
  | "submit", Micheline.Bytes (_, submission) ->
      let* submission = BLAKE2B.of_bytes submission in
      Some (Submit submission)
  | ( "fork_reject",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [ Micheline.String (_, commiter); String (_, rejector) ],
          _ ) ) ->
      let open Tezos.Address in
      let* commiter = of_string commiter and* rejector = of_string rejector in
      Some (Fork_reject { commiter; rejector })
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
      let* committer = of_string committer
      and* defend_as = of_string defend_as
      and* mid_state_hash =
        BLAKE2B.of_string (Bytes.to_string mid_state_hash)
      in
      Some (Reject { level; committer; defend_as; mid_state_hash })
  | ( "defend",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [
            Prim
              (_, D_Pair, [ Micheline.Int (_, level); String (_, rejector) ], _);
            prim;
          ],
          _ ) ) ->
      let* move = Rejection_game.defend_of_micheline prim
      and* rejector = Address.of_string rejector in
      Some (Defend { level; move; rejector })
  | ( "attack",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [
            Prim
              (_, D_Pair, [ Micheline.Int (_, level); String (_, commiter) ], _);
            prim;
          ],
          _ ) ) ->
      let* move = Rejection_game.attack_of_micheline prim
      and* committer = Address.of_string commiter in
      Some (Attack { level; committer; move })
  | ( "trust_commit",
      Micheline.Prim
        ( _,
          Michelson_v1_primitives.D_Pair,
          [ Micheline.Int (_, level); String (_, committer) ],
          _ ) ) ->
      let* committer = Address.of_string committer in
      Some (Trust_commit { level; committer })
  | _ -> None
