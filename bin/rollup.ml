open Cmdliner

module Commit = struct
  open Helpers
  open Crypto

  type t = { level : Z.t; state_hash : BLAKE2B.t } [@@deriving yojson]
end

module Side_effects_interop = struct
  type t = Commit.t [@@deriving yojson]

  let run ~context ~entrypoint ~payload =
    let open Lwt.Syntax in
    let* _ =
      Tezos_interop.Run_contract.run ~context
        ~destination:context.consensus_contract ~entrypoint ~payload
    in
    Lwt.return_unit

  let execute : context:Tezos_interop.Context.t -> t -> unit Lwt.t =
   fun ~context payload ->
    let open Lwt.Syntax in
    let payload = to_yojson payload in
    let* () = run ~context ~entrypoint:"commit" ~payload in
    Lwt.return_unit
end

let node folder =
  let open Lwt.Syntax in
  let* interop_context =
    Files.Interop_context.read ~file:(folder ^ "/tezos.json")
  in
  let callback : Tezos_interop.Listener.operation -> unit =
   fun oc ->
    match oc.parameters with
    | Commit { level; state_hash } ->
        Lwt.async (fun _ ->
            Side_effects_interop.execute ~context:interop_context
              { Commit.level; state_hash })
    | _ -> ()
  in
  let () =
    Tezos_interop.Listener.listen
      ~listener:(module Tezos_interop.OpsListener)
      ~context:interop_context ~on_operation:callback
  in
  let () =
    Tezos_interop.Listener.listen
      ~listener:(module Tezos_interop.BlockOpsListener)
      ~context:interop_context ~on_operation:callback
  in
  Lwt.return_unit

let node folder =
  let () = node folder |> Lwt_main.run in
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
