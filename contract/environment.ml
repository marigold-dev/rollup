include Tezos_environment

type state_hash = bytes
type level = nat
type steps = nat

type committer = address
type rejector = address

let current_level = Tezos.level
let sender = Tezos.sender

let round_time = 10n
