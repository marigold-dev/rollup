open Cmdliner

let node folder =
  let open Lwt.Syntax in
  let* interop_context =
    Files.Interop_context.read ~file:(folder ^ "/tezos.json")
  in
  let () =
    Tezos_interop.Consensus.listen_operations ~context:interop_context
      ~on_operation:(fun _operation -> ())
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
