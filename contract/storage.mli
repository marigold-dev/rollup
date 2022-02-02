open Environment

module Level_data : sig
  type t

  (* O(1) *)
  val input_hash : t -> input_hash

  (* O(1) *)
  val commits : t -> nat
end

module Commit_data : sig
  type t

  (* O(1) *)
  val previous_state_hash : t -> state_hash

  (* O(1) *)
  val state_hash : t -> state_hash

  (* O(1) *)
  val steps : t -> steps

  (* O(1) *)
  val games : t -> nat
end

(* TODO: explain that collect works only with dangling values *)

(* TODO: invariant no data that had it's parent removed should be retrievable *)
(* TODO: for the above to work, no data that was ever removed should ever be added again *)

type t

(* O(1) *)
val make :
  initial_trusted_state_hash:state_hash -> initial_trusted_level:level -> t

(* O(1) *)
val has_garbage : t -> bool

(* O(log n) *)
val append_action : current_level:level -> action_hash:action_hash -> t -> t

(* O(log n) *)
val find_level : level:level -> t -> Level_data.t

(* O(log n) *)
val remove_level : level:level -> t -> t option

(* O(log n) *)
val append_commit :
  level:level ->
  committer:committer ->
  previous_state_hash:state_hash ->
  state_hash:state_hash ->
  steps:steps ->
  t ->
  t option

(* O(log n) *)
val find_commit :
  level:level -> committer:committer -> t -> Commit_data.t option

(* O(log n) *)
val remove_commit : level:level -> committer:committer -> t -> t option

(* O(log n) *)
val collect_commit : level:level -> committer:committer -> t -> t option

(* O(log n) *)
val append_game :
  level:level ->
  committer:committer ->
  rejector:rejector ->
  Temporal_rejection_game.t ->
  t ->
  t option

(* O(log n) *)
val update_game :
  level:level ->
  committer:committer ->
  rejector:rejector ->
  Temporal_rejection_game.t ->
  t ->
  t option

(* O(log n) *)
val find_game :
  level:level ->
  committer:committer ->
  rejector:rejector ->
  t ->
  Temporal_rejection_game.t option

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
