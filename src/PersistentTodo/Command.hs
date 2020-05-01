module PersistentTodo.Command
  ( Command(..)
  , Place(..)
  )
where

import           PersistentTodo.Task            ( Title
                                                , Status
                                                )

data Command
  = Add Title
  | Set Status Place
  | Move Place Place
  | Remove Place
  | Clean
  | Wipe

newtype Place = Place Int
