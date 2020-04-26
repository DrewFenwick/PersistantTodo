module PersistentTodo.Command
  ( Command(..)
  )
where

data Command
  = Add String
  | Complete Int
  | Move Int Int
  | Remove Int
  | Clean
  | Wipe
  deriving (Show)
