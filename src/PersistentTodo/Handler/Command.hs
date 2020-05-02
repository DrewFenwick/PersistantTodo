module PersistentTodo.Handler.Command
  ( Command(..)
  )
where

import           PersistentTodo.Task            ( Title
                                                , Status
                                                , Place
                                                )

data Command
  = Add Title
  | Set Status Place
  | Move Place Place
  | Remove Place
  | Clean
  | Wipe
