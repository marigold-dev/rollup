open Crypto

type t = int
let run_submissions ?until_step submissions vm_state =
  let rec go steps submissions vm_state =
    match submissions with
    | (hd :: tl) :: rest ->
        let do_computation =
          Option.is_none until_step || steps < Option.get until_step
        in
        if do_computation then
          let vm_state = hd + vm_state in
          go (steps + 1) (tl :: rest) vm_state
        else (vm_state, steps)
    | [] :: rest -> go steps rest vm_state
    | [] -> (vm_state, steps)
  in
  go 0 submissions vm_state

let hash_state state = state |> Int.to_string |> BLAKE2B.hash
