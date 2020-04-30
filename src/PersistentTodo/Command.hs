module PersistentTodo.Command
  ( Command(..)
  , Place(..)
  )
where

import           PersistentTodo.Task            ( Title )

data Command
  = Add Title
  | Complete Place
  | Move Place Place
  | Remove Place
  | Clean
  | Wipe

newtype Place = Place Int
