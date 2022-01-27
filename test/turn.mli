open Environment

type t

(* O(1) *)
val first_committer_turn : t

(* O(1) *)
val first_rejector_turn : t

(* O(1) *)
val current : current_level:level -> level:level -> t

type kind = private
  | Turn_committer
  | Turn_fork_committer
  | Turn_rejector
  | Turn_fork_rejector

(* O(1) *)
val kind : t -> kind

(* O(1) *)
(* TODO: this is clearly temporal_rejection_game logic *)
val must_timeout : last_opposite_player_turn:t -> current_turn:t -> bool

(* O(1) *)
(* TODO: this is clearly temporal_rejection_game logic *)
val should_undo_move : last_player_turn:t -> current_turn:t -> bool
