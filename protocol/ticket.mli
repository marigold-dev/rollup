type t = Tezos.Ticket_id.t = { ticketer : Tezos.Address.t; data : bytes }

val compare : t -> t -> Ppx_deriving_runtime.int

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
