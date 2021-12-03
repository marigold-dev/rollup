open Helpers
open Tezos
open Tezos_interop

exception Invalid_json of string

let read_json of_yojson ~file =
  let open Lwt in
  let open Lwt.Syntax in
  let* str = Lwt_io.with_file ~mode:Input file Lwt_io.read in
  let json = Yojson.Safe.from_string str in
  match of_yojson json with
  | Ok data -> return data
  | Error error -> raise (Invalid_json error)

let write_json to_yojson data ~file =
  let callback oc =
    Lwt_io.write oc (Yojson.Safe.pretty_to_string (to_yojson data))
  in
  Lwt_io.with_file ~mode:Output file callback

module type Rw = sig
  type t

  val read : file:string -> t Lwt.t

  val write : t -> file:string -> unit Lwt.t
end

module Interop_context = struct
  module Secret = struct
    include Crypto.Secret

    let of_yojson = function
      | `String string ->
          of_string string |> Option.to_result ~none:"invalid secret"
      | _ -> Error "expected a string"

    let to_yojson t = `String (to_string t)
  end

  type t = Interop_context.t = {
    rpc_node : Uri.t;
    secret : Secret.t;
    consensus_contract : Address.t;
  }
  [@@deriving yojson]

  let read = read_json of_yojson

  let write = write_json to_yojson
end

module State_bin = struct
  let read ~file = Lwt_io.with_file ~mode:Input file Lwt_io.read_value

  let write protocol ~file =
    let cb out = Lwt_io.write_value out protocol in
    Lwt_io.with_file ~mode:Output file cb
end
