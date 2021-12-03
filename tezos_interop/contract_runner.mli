open Tezos
module Output : sig
  type t =
    | Applied of { hash : string }
    | Failed of { hash : string }
    | Skipped of { hash : string }
    | Backtracked of { hash : string }
    | Unknown of { hash : string }
    | Error of string
end

val run :
  context:Interop_context.t ->
  destination:Address.t ->
  entrypoint:string ->
  payload:Yojson.Safe.t ->
  Output.t Lwt.t
