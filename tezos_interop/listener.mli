open Tezos

type kind = Blocks | Transactions
module Operation : sig
  type t = private {
    hash : Operation_hash.t;
    index : int;
    parameters : Entrypoint_param.t;
  }
end
val listen :
  kind:kind ->
  context:Interop_context.t ->
  on_operation:(Operation.t -> unit) ->
  unit
