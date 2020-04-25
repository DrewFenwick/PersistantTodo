module PersistentTodo.Command where

import PersistentTodo.Task

data Command
  = Add Task
  | Complete Int
  | Move Int Int
  | Remove Int
  | Clean
  | Wipe