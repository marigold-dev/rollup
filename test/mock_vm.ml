open Ligo_environment

type t = bytes list

let make ~hashes = hashes

let single_step_data t =
  match t with
  | fst :: snd :: _ -> Some [fst; snd]
  | fst :: _ -> Some [fst]
  | [] -> None

let execute_step t =
  match t with
  | [] -> []
  | _hd :: tl -> tl

let halted = Pack.nat [%nat 1]
let hash t =
  match t with
  | hd :: _ -> hd
  | [] -> halted
