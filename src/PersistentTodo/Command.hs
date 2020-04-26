module PersistentTodo.Command
  ( Command(..)
  , Place(..)
  )
where

data Command
  = Add String
  | Complete Place
  | Move Place Place
  | Remove Place
  | Clean
  | Wipe

newtype Place = Place Int
