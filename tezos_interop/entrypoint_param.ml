open Helpers
open Crypto
open Tezos
type t =
  | Submit of Bytes.t
  | Commit of { level : Z.t; state_hash : BLAKE2B.t; step : Z.t }
  | Join
  | Exit
  | Fork_game of { commiter : Address.t; rejector : Address.t }

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
          [ Int (_, _level); Bytes (_, _state_hash) ],
          _ ) ) ->
      None
  | _ -> None
