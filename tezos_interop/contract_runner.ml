open Helpers
open Crypto
open Tezos
open Scripts

module Input = struct
  type t = {
    rpc_node : string;
    secret : string;
    destination : string;
    entrypoint : string;
    payload : Yojson.Safe.t;
  }
  [@@deriving to_yojson]
end
module Output = struct
  type t =
    | Applied of { hash : string }
    | Failed of { hash : string }
    | Skipped of { hash : string }
    | Backtracked of { hash : string }
    | Unknown of { hash : string }
    | Error of string

  module Apply = struct
    let applied hash = Applied { hash }
    let failed hash = Applied { hash }
    let skipped hash = Applied { hash }
    let backtracked hash = Applied { hash }
    let unknown hash = Applied { hash }
    let error err = Error err
  end

  let of_yojson =
    let open Result.Syntax in
    let module T = struct
      type t = { status : string } [@@deriving of_yojson { strict = false }]

      and finished = { hash : string }

      and error = { error : string }
    end in
    let finished json make =
      let* { hash } = T.finished_of_yojson json in
      Ok (make hash)
    in
    fun json ->
      let finished = finished json in
      let* { status } = T.of_yojson json in
      let open Apply in
      match status with
      | "applied" -> finished applied
      | "failed" -> finished failed
      | "skipped" -> finished skipped
      | "backtracked" -> finished backtracked
      | "unknown" -> finished unknown
      | "error" ->
          let* { error = err; _ } = T.error_of_yojson json in
          Ok (error err)
      | _ -> Error "invalid status"
end

module Script_file = Make_script_file (Entrypoint_script) ()

let run ~(context : Interop_context.t) ~destination ~entrypoint ~payload =
  let open Lwt in
  let open Lwt.Syntax in
  let input : Input.t =
    {
      rpc_node = context.rpc_node |> Uri.to_string;
      secret = context.secret |> Secret.to_string;
      destination = Address.to_string destination;
      entrypoint;
      payload;
    }
  in
  let* output =
    let open Script_file in
    Lwt_process.pmap
      (command, [| command; file |])
      (Yojson.Safe.to_string @@ Input.to_yojson input)
  in
  match Yojson.Safe.from_string output |> Output.of_yojson with
  | Ok data -> return data
  | Error error -> return (Output.Error error)
