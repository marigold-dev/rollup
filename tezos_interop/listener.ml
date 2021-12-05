open Helpers
open Tezos
open Scripts
module M = struct
  module Michelson = struct
    open Tezos_micheline
    open Data_encoding
    open Pack
    type t = (int, Michelson_v1_primitives.prim) Micheline.node

    let of_yojson json =
      let open Result.Syntax in
      let* json = json |> Yojson.Safe.to_string |> Json.from_string in
      try Ok (Json.destruct expr_encoding json |> Micheline.root)
      with _ -> Error "invalid json"
  end

  module Output = struct
    type t = {
      hash : string;
      index : int;
      level : int option;
      entrypoint : string;
      value : Michelson.t;
    }
    [@@deriving of_yojson]
  end

  module Operation = struct
    type t = {
      hash : Operation_hash.t;
      index : int;
      parameters : Entrypoint_param.t;
    }
    let of_output (output : Output.t) =
      let open Option.Syntax in
      let* parameters =
        Entrypoint_param.of_micheline output.entrypoint output.value
      in
      let* hash = Operation_hash.of_string output.hash in
      Some { hash; index = output.index; parameters }
  end

  module CLI (S : Script) = struct
    include Make_script_file (S) ()
    module Input = struct
      type t = { rpc_node : string; destination : string }
      [@@deriving to_yojson]
    end
    let run ~(context : Interop_context.t) ~destination ~on_message ~on_fail =
      let open Lwt.Syntax in
      let send f pr data =
        let oc = pr#stdin in
        Lwt.finalize (fun _ -> f oc data) (fun _ -> Lwt_io.close oc)
      in
      let process = Lwt_process.open_process (command, [| command; file |]) in
      let input =
        {
          Input.rpc_node = Uri.to_string context.rpc_node;
          destination = Address.to_string destination;
        }
        |> Input.to_yojson |> Yojson.Safe.to_string
      in
      let on_fail _exn =
        let* _status = process#close in
        on_fail ()
      in

      let* () = send Lwt_io.write process input in

      let rec read_line_until_fails () =
        Lwt.catch
          (fun _ ->
            let* line = Lwt_io.read_line process#stdout in
            let _ = on_message in
            print_endline line;
            (* Yojson.Safe.from_string line
               |> Output.of_yojson |> Result.get_ok |> on_message; *)
            read_line_until_fails ())
          on_fail
      in
      read_line_until_fails ()
  end

  module Make_reader (S : Script) = struct
    module CLI = CLI (S)
    let listen ~context ~destination ~on_message =
      let rec start () =
        Lwt.catch
          (fun () -> CLI.run ~context ~destination ~on_message ~on_fail)
          (fun _exn -> on_fail ())
      and on_fail () = start () in
      Lwt.async start
  end
  module type Aux = sig
    val listen :
      context:Interop_context.t ->
      destination:Address.t ->
      on_message:(Output.t -> unit) ->
      unit
  end
  module Block_listener = Make_reader (Scripts.Blocks_script)
  module Transactions_listener = Make_reader (Scripts.Transactions_script)
end
open M
module Operation = Operation

type kind = Blocks | Transactions

let listen ~kind ~context ~on_operation =
  let l =
    match kind with
    | Blocks -> (module Block_listener : Aux)
    | Transactions -> (module Transactions_listener : Aux)
  in
  let module L = (val l) in
  let on_message output =
    match Operation.of_output output with
    | Some operation -> on_operation operation
    | None -> ()
  in
  L.listen ~context ~destination:context.consensus_contract ~on_message
