open Common

type action

type t

let execute_step : t -> t = assert false

let apply : action -> t -> t = assert false

let hash : t -> state_hash = assert false

let step : t -> step = assert false
