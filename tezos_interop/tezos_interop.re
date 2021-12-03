module Entrypoint_param = Entrypoint_param;
module Interop_context = Interop_context;
module Contract_runner = Contract_runner;
module Listener = Listener;

// can be deleted?
module Discovery = {
  open Helpers;
  open Crypto;

  open Tezos;
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
