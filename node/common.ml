type level = int
type hash = Crypto.BLAKE2B.t
type submission = int list
type author = string
type step = int
type step_count = int
type commit =Commit of {
  level: level;
  author: author;
  hash: hash;
  step_count: step_count
}

type block_data = {
  level : level;
  submissions : submission list;
  commits : commit list;
}
