open Environment

module Commit_data : sig
  type t

  (* O(1) *)
  val make :
    parent_state_hash:state_hash -> state_hash:state_hash -> steps:steps -> t

  (* O(1) *)
  val parent_state_hash : t -> state_hash

  (* O(1) *)
  val state_hash : t -> state_hash

  (* O(1) *)
  val steps : t -> steps

  (* O(1) *)
  val games : t -> nat
end

(* TODO: explain that collect works only with dangling values *)
type game
type t

(* O(1) *)
val make :
  initial_trusted_state_hash:state_hash -> initial_trusted_level:level -> t

(* O(1) *)
val has_garbage : t -> bool

(* O(log n) *)
val remove_level : level:level -> t -> t option

(* O(log n) *)
val append_commit :
  level:level ->
  committer:committer ->
  commit_data:Commit_data.t ->
  t ->
  t option

(* O(log n) *)
val remove_commit : level:level -> committer:committer -> t -> t option

(* O(log n) *)
val collect_commit : level:level -> committer:committer -> t -> t option

(* O(log n) *)
val append_game :
  level:level ->
  committer:committer ->
  rejector:rejector ->
  game ->
  t ->
  t option

(* O(log n) *)
val remove_game :
  level:level -> committer:committer -> rejector:rejector -> t -> t option

(* O(log n) *)
val collect_game :
  level:level -> committer:committer -> rejector:rejector -> t -> t option

(* O(1) *)
val trust_level : level:level -> t -> t

(* O(1) *)
val trusted_level : t -> level

(* O(1) *)
val trust_state_hash : state_hash -> t -> t

(* O(1) *)
val trusted_state_hash : t -> state_hash

(* O(log n) *)
val has_stake : address -> t -> bool

(* O(log n) *)
val join : address -> t -> t

(* O(log n) *)
val burn : address -> t -> t
