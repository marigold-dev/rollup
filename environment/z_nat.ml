open Z_int

type nat = int
let nat n =
  assert (Stdlib.(n >= 0));
  int n

let ( + ) = ( + )
let ( - ) = ( - )
let ( * ) = ( * )
let ( < ) = ( < )
let ( >= ) = ( >= )

let abs = abs
let ediv = ediv
let min (a : nat) (b : nat) = min a b

module Read = struct
  let to_z = Read.to_z
end
