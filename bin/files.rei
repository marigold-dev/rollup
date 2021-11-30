open Node;
open State;

exception Invalid_json(string);

module Identity: {
  let read: (~file: string) => Lwt.t(identity);
  let write: (identity, ~file: string) => Lwt.t(unit);
};

module Interop_context: {
  let read: (~file: string) => Lwt.t(Tezos_interop.Context.t);
  let write: (Tezos_interop.Context.t, ~file: string) => Lwt.t(unit);
};

module State_bin: {
  let read: (~file: string) => Lwt.t(Protocol.t);
  let write: (Protocol.t, ~file: string) => Lwt.t(unit);
};
