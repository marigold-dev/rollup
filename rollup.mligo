type level = nat
type payload = bytes
type state_hash = bytes

type submission = {payload : payload}
type commitment = {new_state_hash : state_hash; level : level }
type rejection = {operation_id : int; proof : bytes}

type parameter =
| Submit of submission
| Commit of commitment
| Reject of rejection

type storage = {
   levels : (level, submission list) big_map;
   commitment: (level, (level * state_hash) set) big_map;
}

let submit (submission: submission) (storage: storage) =
  let {levels; commitment} = storage in
  let current_submissions =
    match Big_map.find_opt Tezos.level levels with
    | Some current_submissions -> current_submissions
    | None -> [] in
  let submissions = submission :: current_submissions in
  let levels = Big_map.add current_level submissions levels in
  {levels; commitment}

let round_time = 10n
let commitment_amount = 1000tz

let commit (commitment: commitment) (storage: storage) =
  let () = if Tezos.amount < commitment_amount then failwith "pleb" in
  // fail if level <= current level
  let {levels; commitment} = storage in
  let {new_state_hash; level} = commitment in
  let level_commitments =
    match Big_map.find_opt commitment.level commitments with
    | Some current_commitments -> current_commitments
    | None -> []
  in
  let level_commitments = Set.add commitment current_commitments in
  let commitments = Big_map.add commitment.level level_commitments  in




let main (action, store : parameter * storage) =
 match action with
 | Submit submission -> ([]: operation list, submit submission storage)