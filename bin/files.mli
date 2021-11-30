exception Invalid_json of string

module Interop_context : sig
  val read : file:string -> Tezos_interop.Context.t Lwt.t

  val write : Tezos_interop.Context.t -> file:string -> unit Lwt.t
end
