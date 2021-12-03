module type Script = sig
  val script : string
end
module Blocks_script : Script
module Transactions_script : Script
module Entrypoint_script : Script
module Make_script_file (S : Script) () : sig
  val command : string
  val file : string
end
