open Crypto
open Tezos
type t = { rpc_node : Uri.t; secret : Secret.t; consensus_contract : Address.t }
