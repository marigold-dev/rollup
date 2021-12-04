open Common
type t =
  | Do_nothing
  | Send_commit of { level : level; hash : hash; step_count : step }
  | Open_rejection_game of {
      level : level;
      accused_author : author;
      midpoint_hash : hash;
      endorsed_commit : commit;
    }
    