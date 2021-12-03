open Crypto

type t

val of_wallet : Key.t -> t
val pubkey_matches_wallet : Crypto.Key.t -> t -> bool

val compare : t -> t -> int
val make : unit -> Secret.t * t

val to_key_hash : t -> Key_hash.t
val of_key_hash : Key_hash.t -> t

val to_string : t -> string
val of_string : string -> t option

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
