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
    (* TODO: actually check also for bounds in compile time *)
    if int < 0 then
      Location.raise_errorf ~loc "%s cannot be negative" name;
    let constant = Exp.constant ~loc (Const.int (int_of_string int_string)) in
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

module Int = Number (struct
  let name = "int"
  let funct_typ ~loc = [%expr int]
end)
module Nat = Number (struct
  let name = "nat"
  let funct_typ ~loc = [%expr nat]
end)
module Nat32 = Number (struct
  let name = "nat32"
  let funct_typ ~loc = [%expr Nat32.of_stdlib_int_exn]
end)
module Tez = Number (struct
  let name = "tez"
  let funct_typ ~loc = [%expr tez]
end)

module Bytes = struct
  let name = "bytes"
  let transform ~loc ~path:_ string _attributes =
    let open Ast_helper in
    let _ =
      try Hex.to_string (`Hex string) with
      | Invalid_argument _ ->
        Location.raise_errorf ~loc "%s is not a hex" string in

    let constant = Exp.constant (Const.string string) in
    [%expr bytes [%e constant]]

  (* driver *)
  let pattern =
    Ast_pattern.(
      pstr (pstr_eval (pexp_constant (pconst_string __ __ __)) __ ^:: nil)
      >>| fun f payload _ _ -> f payload)

  let rule =
    Extension.declare name Extension.Context.expression pattern transform
    |> Context_free.Rule.extension
end

let () =
  Driver.register_transformation "ppx_ligo_interop"
    ~rules:[Int.rule; Nat.rule; Nat32.rule; Tez.rule; Bytes.rule]
