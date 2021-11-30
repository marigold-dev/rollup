// TODO: does computers have multiple RTC nowadays?
// Can a core send a message and the other receive it in the past?
// TODO: should start signing before being in sync?

open Cmdliner;
// open Opium;
open Helpers;
// open Protocol;
open Node;

let _ignore_some_errors =
  fun
  | Error(#Flows.ignore) => Ok()
  | v => v;
let _print_error = err => {
  open Format;
  switch (err) {
  | `Added_block_has_lower_block_height =>
    eprintf("Added block has lower block height")
  | `Added_block_not_signed_enough_to_desync =>
    eprintf("Added_block_not_signed_enough_to_desync")
  | `Added_signature_not_signed_enough_to_request =>
    eprintf("Added_signature_not_signed_enough_to_request")
  | `Already_known_block => eprintf("Already_known_block")
  | `Already_known_signature => eprintf("Already_known_signature")
  | `Block_not_signed_enough_to_apply =>
    eprintf("Block_not_signed_enough_to_apply")
  | `Failed_to_verify_payload => eprintf("Failed to verify payload signature")
  | `Invalid_address_on_main_operation =>
    eprintf("Invalid_address_on_main_operation")
  | `Invalid_block(string) => eprintf("Invalid_block(%s)", string)
  | `Invalid_block_when_applying => eprintf("Invalid_block_when_applying")
  | `Invalid_nonce_signature => eprintf("Invalid_nonce_signature")
  | `Invalid_signature_author => eprintf("Invalid_signature_author")
  | `Invalid_signature_for_this_hash =>
    eprintf("Invalid_signature_for_this_hash")
  | `Invalid_state_root_hash => eprintf("Invalid_state_root_hash")
  | `Not_current_block_producer => eprintf("Not_current_block_producer")
  | `Not_a_json => eprintf("Invalid json")
  | `Not_a_valid_request(err) => eprintf("Invalid request: %s", err)
  | `Pending_blocks => eprintf("Pending_blocks")
  | `Unknown_uri => eprintf("Unknown_uri")
  };
  eprintf("\n%!");
};

let node = folder => {
  let.await identity = Files.Identity.read(~file=folder ++ "/identity.json");
  let.await interop_context =
    Files.Interop_context.read(~file=folder ++ "/tezos.json");
  Tezos_interop.Consensus.listen_operations(
    ~context=interop_context, ~on_operation=_operation =>
    print_endline("ass")
  );
  Lwt.return_unit;
};

let node = folder => {
  let () = node(folder) |> Lwt_main.run;
  let (forever, _) = Lwt.wait();
  Lwt_main.run(forever);
};

let node = {
  let folder_node = {
    let docv = "folder_node";
    let doc = "Path to the folder containing the node configuration data.";
    Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
  };

  Term.(const(node) $ folder_node);
};

let () = {
  Term.exit @@ Term.eval((node, Term.info("deku-node")));
};
