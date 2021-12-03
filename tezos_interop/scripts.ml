module type Script = sig
  val script : string
end
module Blocks_script : Script = struct
  let script = [%blob "listen_blocks.bundle.js"]
end
module Transactions_script : Script = struct
  let script = [%blob "listen_transactions.bundle.js"]
end
module Entrypoint_script : Script = struct
  let script = [%blob "run_entrypoint.bundle.js"]
end

module Make_script_file (S : Script) () : sig
  val command : string
  val file : string
end = struct
  include S
  let file =
    let open Lwt in
    let open Lwt.Syntax in
    let* file, oc = Lwt_io.open_temp_file ~suffix:".js" () in
    let* () = Lwt_io.write oc script in
    return file
  let command = "node"
  let file = Lwt_main.run file
end
