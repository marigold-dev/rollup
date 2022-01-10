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
val empty : unit -> t

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
