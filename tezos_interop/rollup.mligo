type vote = 
 | Agree 
 | Disagree
type new_reject_game = {level: nat; commiter: address; r_m_h: bytes; r_s_h: bytes; steps: nat }
type actions = 
| Commit of {level: nat; state_hash: bytes; steps: nat}
| Reject of {level: nat; commiter: address; r_m_h: bytes; r_s_h: bytes; steps: nat }
| Fork_commit of {commiter: address}
| Fork_game of {commiter: address; rejector: address}
| Start_rejection_game of new_reject_game
| Send_middle_hash of { committer : address; state_hash : bytes }
| Vote_on_middle of { rejector : address; vote : vote }
| Replay of { committer : address; state : bytes }

type storage = unit
type return = operation list * storage

let main (action, storage: actions * storage): return = 
 match action with 
| Commit {level = _; state_hash = _; steps = _} -> ([]: operation list), storage
| _ -> ([]: operation list), storage
