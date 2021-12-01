type level = nat
type storage = {current_level: level; state: bytes list}
type submission = bytes
type state_hash = bytes

type action =
  | Submit of submission
  | Join
  | Commit of level * state_hash


type return = operation list * storage

let enact (store, delta : storage * bytes) : storage = 
    {current_level = store.current_level + 1n; state = delta :: store.state}
let main (action, store : action * storage) : return =
 ([] : operation list), 
 (match action with
   Submit (byt) -> enact(store, byt)
 | Commit (_, byt) -> enact(store, byt)
 | _         -> store)
