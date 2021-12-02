open Helpers;
open Crypto;
open Tezos;

module Context: {
  type t = {
    rpc_node: Uri.t,
    secret: Secret.t,
    consensus_contract: Address.t,
  };
};

module Run_contract: {
  type output =
    | Applied({hash: string})
    | Failed({hash: string})
    | Skipped({hash: string})
    | Backtracked({hash: string})
    | Unknown({hash: string})
    | Error(string);

  let run:
    (
      ~context: Context.t,
      ~destination: Address.t,
      ~entrypoint: string,
      ~payload: Yojson.Safe.t
    ) =>
    Lwt.t(output);
};
module Listener_output: {type t;};
module type Listener = {
  let listen:
    (
      ~context: Context.t,
      ~destination: Address.t,
      ~on_message: Listener_output.t => unit
    ) =>
    unit;
};
module BlockOpsListener: Listener;
module OpsListener: Listener;

module Listener: {
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
  type listener =
    | Blocks
    | Ops;

  let listen:
    (
      ~listener: (module Listener),
      ~context: Context.t,
      ~on_operation: operation => unit
    ) =>
    unit;
};

module Discovery: {let sign: (Secret.t, ~nonce: int64, Uri.t) => Signature.t;};
