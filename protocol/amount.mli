type t [@@deriving yojson]
val compare : t -> t -> t
val zero : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val of_int : int -> t
val to_int : t -> int
