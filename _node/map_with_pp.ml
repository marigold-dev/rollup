module Make (P : sig
  include Map.OrderedType
  val pp : Format.formatter -> t -> unit
end) =
struct
  include Map.Make (P)
  let pp pp_value fmt t =
    Format.fprintf fmt "%s" ([%show: (P.t * value) list] (bindings t))
end
