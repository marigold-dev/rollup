type t = Tezos.Ticket_id.t = { ticketer : Tezos.Address.t; data : bytes }

val compare : t -> t -> int

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result
