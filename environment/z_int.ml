type int = Z.t
let int = Z.of_int

let ( + ) = Z.( + )
let ( - ) = Z.( - )
let ( * ) = Z.( * )
let ( < ) = ( < )
let ( >= ) = ( >= )

let abs = Z.abs
let ediv a b = if Z.equal b Z.zero then None else Some (Z.div_rem a b)

module Read = struct
  let to_z x = x
end
