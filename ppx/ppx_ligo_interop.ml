open Ppxlib

module Number (P : sig
  val name : string
  val funct_typ : loc:Location.t -> expression
end) =
struct
  open P
  let transform ~loc ~path:_ int_string =
    let open Ast_helper in
    let int = int_of_string int_string in
    if int < 0 then
      Location.raise_errorf ~loc "%s cannot be negative" name;
    let constant = Exp.constant (Const.int (int_of_string int_string)) in
    [%expr [%e funct_typ ~loc] [%e constant]]

  (* driver *)
  let pattern =
    Ast_pattern.(
      pstr (pstr_eval (pexp_constant (pconst_integer __ __)) __ ^:: nil)
      >>| fun f payload _ _ -> f payload)

  let rule =
    Extension.declare name Extension.Context.expression pattern transform
    |> Context_free.Rule.extension
end

module Nat = Number (struct
  let name = "nat"
  let funct_typ ~loc = [%expr nat]
end)
module Tez = Number (struct
  let name = "tez"
  let funct_typ ~loc = [%expr tez]
end)

let () =
  Driver.register_transformation "ppx_ligo_interop" ~rules:[Nat.rule; Tez.rule]
