module Command where

data Command
  = Add Task
  | Complete Int
  | Move Int Int
  | Remove Int
  | Clean
  | Wipe