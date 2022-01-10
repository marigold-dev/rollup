module Bytes = struct
  let concat a b = Bytes.concat a [ b ]
  let of_string = Bytes.of_string
end

module Z = struct
  include Z
  let pp = pp_print
end
type nat = Z.t
type address = Address.t [@@deriving show { with_path = false }]

type player = Rejector | Committer [@@deriving show { with_path = false }]
type committer = address [@@deriving show { with_path = false }]
type rejector = address [@@deriving show { with_path = false }]
let pp_nat = Z.pp_print
type hash = bytes
let pp_hash fmt hash =
  let (`Hex hash) = Hex.of_bytes hash in
  Format.fprintf fmt "0x%s" hash

type state_hash = hash [@@deriving show { with_path = false }]

type level = Z.t [@@deriving show { with_path = false }]
type steps = nat [@@deriving show { with_path = false }]

include Z
