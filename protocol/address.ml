open Crypto
open Key_hash
type t = Key_hash.t [@@deriving ord]

let of_wallet = of_key
let pubkey_matches_wallet key t = equal (of_key key) t

let make () =
  let key, pub_ = Ed25519.generate () in
  let wallet_address = of_wallet (Ed25519 pub_) in
  (Secret.Ed25519 key, wallet_address)
let to_key_hash t = t
let of_key_hash t = t

let to_string = to_string
let of_string = of_string

let to_yojson = to_yojson
let of_yojson = of_yojson
