open Cmdliner

module Commit = struct
  open Helpers
  open Crypto

  type t = { level : Z.t; state_hash : BLAKE2B.t } [@@deriving yojson]
end

module Side_effects_interop = struct
  open Tezos_interop
  type t = Commit.t [@@deriving yojson]

  let run ~context ~entrypoint ~payload =
    let open Lwt.Syntax in
    let* _ =
      Contract_runner.run ~context ~destination:context.consensus_contract
        ~entrypoint ~payload
    in
    Lwt.return_unit

  let execute : context:Interop_context.t -> t -> unit Lwt.t =
   fun ~context payload ->
    let open Lwt.Syntax in
    let payload = to_yojson payload in
    let* () = run ~context ~entrypoint:"commit" ~payload in
    Lwt.return_unit
end
module Node = struct
  open Tezos_interop
  let apply folder =
    let open Lwt.Syntax in
    let* interop_context =
      Files.Interop_context.read ~file:(folder ^ "/tezos.json")
    in
    let callback (oc : Listener.Operation.t) =
      match oc.parameters with
      | Entrypoint_param.Commit { level; state_hash; _ } ->
          Lwt.async (fun _ ->
              Side_effects_interop.execute ~context:interop_context
                { Commit.level; state_hash })
      | _ -> ()
    in

    let () =
      Listener.listen ~kind:Blocks ~context:interop_context
        ~on_operation:callback
    in
    Lwt.return_unit
end
let node folder =
  let () = Node.apply folder |> Lwt_main.run in
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let node =
  let folder_node =
    let docv = "folder_node" in
    let doc = "Path to the folder containing the node configuration data." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)
  in
  Term.(const node $ folder_node)

let () = Term.exit @@ Term.eval (node, Term.info "rollup-node")
