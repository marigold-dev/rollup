module Make (P : sig
  include Set.OrderedType
  val pp : Format.formatter -> t -> unit
end) =
struct
  include Set.Make (P)
  let pp fmt t = Format.fprintf fmt "%s" ([%show: P.t list] (elements t))
end
