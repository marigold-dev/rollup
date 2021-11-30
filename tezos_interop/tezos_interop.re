open Helpers;
open Crypto;
open Tezos;

module Context = {
  type t = {
    rpc_node: Uri.t,
    secret: Secret.t,
    consensus_contract: Address.t,
  };
};
module Run_contract = {
  [@deriving to_yojson]
  type input = {
    rpc_node: string,
    secret: string,
    destination: string,
    entrypoint: string,
    payload: Yojson.Safe.t,
  };
  type output =
    | Applied({hash: string})
    | Failed({hash: string})
    | Skipped({hash: string})
    | Backtracked({hash: string})
    | Unknown({hash: string})
    | Error(string);

  let output_of_yojson = json => {
    module T = {
      [@deriving of_yojson({strict: false})]
      type t = {status: string}
      and finished = {hash: string}
      and error = {error: string};
    };
    let finished = make => {
      let.ok {hash} = T.finished_of_yojson(json);
      Ok(make(hash));
    };
    let.ok {status} = T.of_yojson(json);
    switch (status) {
    | "applied" => finished(hash => Applied({hash: hash}))
    | "failed" => finished(hash => Failed({hash: hash}))
    | "skipped" => finished(hash => Skipped({hash: hash}))
    | "backtracked" => finished(hash => Backtracked({hash: hash}))
    | "unknown" => finished(hash => Unknown({hash: hash}))
    | "error" =>
      let.ok {error} = T.error_of_yojson(json);
      Ok(Error(error));
    | _ => Error("invalid status")
    };
  };

  // TODO: this leaks the file as it needs to be removed when the app closes
  let file = {
    let.await (file, oc) = Lwt_io.open_temp_file(~suffix=".js", ());
    let.await () = Lwt_io.write(oc, [%blob "run_entrypoint.bundle.js"]);
    await(file);
  };
  let file = Lwt_main.run(file);
  let run = (~context, ~destination, ~entrypoint, ~payload) => {
    let input = {
      rpc_node: context.Context.rpc_node |> Uri.to_string,
      secret: context.secret |> Secret.to_string,
      destination: Address.to_string(destination),
      entrypoint,
      payload,
    };
    // TODO: stop hard coding this
    let command = "node";
    let.await output =
      Lwt_process.pmap(
        (command, [|command, file|]),
        Yojson.Safe.to_string(input_to_yojson(input)),
      );

    switch (Yojson.Safe.from_string(output) |> output_of_yojson) {
    | Ok(data) => await(data)
    | Error(error) => await(Error(error))
    };
  };
};

let michelson_of_yojson = json => {
  // TODO: do this without serializing
  let.ok json = Yojson.Safe.to_string(json) |> Data_encoding.Json.from_string;
  try(
    Ok(
      Tezos_micheline.Micheline.root(
        Data_encoding.Json.destruct(Pack.expr_encoding, json),
      ),
    )
  ) {
  | _ => Error("invalid json")
  };
};
type michelson =
  Tezos_micheline.Micheline.node(int, Pack.Michelson_v1_primitives.prim);

module Listen_transactions = {
  [@deriving of_yojson]
  type output = {
    hash: string,
    index: int,
    entrypoint: string,
    value: michelson,
  };
  module CLI = {
    [@deriving to_yojson]
    type input = {
      rpc_node: string,
      destination: string,
    };
    let file = {
      let.await (file, oc) = Lwt_io.open_temp_file(~suffix=".js", ());
      let.await () =
        Lwt_io.write(oc, [%blob "listen_transactions.bundle.js"]);
      await(file);
    };
    let file = Lwt_main.run(file);

    let node = "node";
    let run = (~context, ~destination, ~on_message, ~on_fail) => {
      let send = (f, pr, data) => {
        let oc = pr#stdin;
        Lwt.finalize(() => f(oc, data), () => Lwt_io.close(oc));
      };

      let process = Lwt_process.open_process((node, [|node, file|]));
      let input =
        {
          rpc_node: Uri.to_string(context.Context.rpc_node),
          destination: Address.to_string(destination),
        }
        |> input_to_yojson
        |> Yojson.Safe.to_string;
      let on_fail = _exn => {
        // TODO: what to do with this exception
        // TODO: what to do with this status
        let.await _status = process#close;
        on_fail();
      };
      let.await () = send(Lwt_io.write, process, input);

      let rec read_line_until_fails = () =>
        Lwt.catch(
          () => {
            let.await line = Lwt_io.read_line(process#stdout);
            print_endline(line);
            Yojson.Safe.from_string(line)
            |> output_of_yojson
            |> Result.get_ok
            |> on_message;
            read_line_until_fails();
          },
          on_fail,
        );
      read_line_until_fails();
    };
  };

  let listen = (~context, ~destination, ~on_message) => {
    let rec start = () =>
      Lwt.catch(
        () => CLI.run(~context, ~destination, ~on_message, ~on_fail),
        // TODO: what to do with this exception?
        _exn => on_fail(),
      )
    and on_fail = () => start();
    Lwt.async(start);
  };
};
module Consensus = {
  open Pack;
  open Tezos_micheline;

  // TODO: how to test this?
  let commit_state_hash =
      (
        ~context,
        ~block_hash,
        ~block_height,
        ~block_payload_hash,
        ~state_hash,
        ~handles_hash,
        ~validators,
        ~signatures,
      ) => {
    module Payload = {
      [@deriving to_yojson]
      type t = {
        block_hash: BLAKE2B.t,
        block_height: int64,
        block_payload_hash: BLAKE2B.t,
        signatures: list(option(string)),
        handles_hash: BLAKE2B.t,
        state_hash: BLAKE2B.t,
        validators: list(string),
      };
    };
    open Payload;
    let signatures =
      // TODO: we should sort the map using the keys
      List.map(
        ((_key, signature)) =>
          Option.map(signature => Signature.to_string(signature), signature),
        signatures,
      );
    let validators = List.map(Key_hash.to_string, validators);
    let payload = {
      block_hash,
      block_height,
      block_payload_hash,
      signatures,
      handles_hash,
      state_hash,
      validators,
    };
    let.await _ =
      Run_contract.run(
        ~context,
        ~destination=context.Context.consensus_contract,
        ~entrypoint="update_root_hash",
        ~payload=Payload.to_yojson(payload),
      );
    await();
  };

  type parameters =
    | Submit(Bytes.t)
    | Commit({
        level: Z.t,
        state_hash: BLAKE2B.t,
      })
    | Join;
  type operation = {
    hash: Operation_hash.t,
    index: int,
    parameters,
  };

  let parse_parameters = (entrypoint, micheline) =>
    switch (entrypoint, micheline) {
    | (
        "commit",
        Micheline.Prim(
          _,
          Michelson_v1_primitives.D_Pair,
          [Int(_, level), Bytes(_, state_hash)],
          _,
        ),
      ) =>
      let.some state_hash =
        state_hash |> Bytes.to_string |> BLAKE2B.of_raw_string;
      Some(Commit({level, state_hash}));
    | ("submit", Micheline.Bytes(_, submission)) =>
      Some(Submit(submission))
    | ("join", _) => Some(Join)
    | _ => None
    };
  let parse_operation = output => {
    let.some parameters =
      parse_parameters(output.Listen_transactions.entrypoint, output.value);
    let.some hash = Operation_hash.of_string(output.hash);
    Some({hash, index: output.index, parameters});
  };
  let listen_operations = (~context, ~on_operation) => {
    let on_message = output => {
      switch (parse_operation(output)) {
      | Some(operation) => on_operation(operation)
      | None => ()
      };
    };
    Listen_transactions.listen(
      ~context,
      ~destination=context.consensus_contract,
      ~on_message,
    );
  };
};

module Discovery = {
  open Pack;

  let sign = (secret, ~nonce, uri) =>
    to_bytes(
      pair(
        int(Z.of_int64(nonce)),
        bytes(Bytes.of_string(Uri.to_string(uri))),
      ),
    )
    |> Bytes.to_string
    |> BLAKE2B.hash
    |> Signature.sign(secret);
};
