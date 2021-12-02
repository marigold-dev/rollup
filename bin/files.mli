open Tezos_interop
open Node

exception Invalid_json of string

module type Rw = sig
  type t

  val read : file:string -> t Lwt.t

  val write : t -> file:string -> unit Lwt.t
end

module Interop_context : Rw with type t := Context.t

module State_bin : Rw with type t := State.t
