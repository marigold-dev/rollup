type t = Crypto.Key.t

val compare : t -> t -> Ppx_deriving_runtime.int

val of_key : Crypto.Secret.t -> t

val genesis_key : Crypto.Secret.t
val genesis_wallet : t

val to_string : t -> string
val of_string : string -> t option

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
