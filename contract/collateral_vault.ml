open Environment

type t = (address, unit) big_map

let has_stake address t = Big_map.mem address t

let join address t = Big_map.add address () t

let burn address t = Big_map.remove address t
