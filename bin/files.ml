open Helpers

exception Invalid_json of string

let read_json of_yojson ~file =
  let open Lwt in
  let open Lwt.Syntax in
  let* string = Lwt_io.with_file ~mode:Input file Lwt_io.read in
  let json = Yojson.Safe.from_string string in
  match of_yojson json with
  | Ok data -> return data
  | Error error -> raise (Invalid_json error)

let write_json to_yojson data ~file =
  Lwt_io.with_file ~mode:Output file (fun oc ->
      Lwt_io.write oc (Yojson.Safe.pretty_to_string (to_yojson data)))

module Interop_context = struct
  module Secret = struct
    include Crypto.Secret

    let of_yojson = function
      | `String string ->
          of_string string |> Option.to_result ~none:"invalid secret"
      | _ -> Error "expected a string"

    let to_yojson t = `String (to_string t)
  end
  [@deriving yojson]

  type t = Tezos_interop.Context.t = {
    rpc_node : Uri.t;
    secret : Secret.t;
    consensus_contract : Tezos.Address.t;
  }
  [@@deriving yojson]

  let read = read_json of_yojson

  let write = write_json to_yojson
end
