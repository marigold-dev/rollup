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

module Consensus: {
  let commit_state_hash:
    (
      ~context: Context.t,
      ~block_hash: BLAKE2B.t,
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_hash: BLAKE2B.t,
      ~handles_hash: BLAKE2B.t,
      ~validators: list(Key_hash.t),
      ~signatures: list((Key_hash.t, option(Signature.t)))
    ) =>
    Lwt.t(unit);

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
  let listen_operations:
    (~context: Context.t, ~on_operation: operation => unit) => unit;
};

module Discovery: {let sign: (Secret.t, ~nonce: int64, Uri.t) => Signature.t;};
