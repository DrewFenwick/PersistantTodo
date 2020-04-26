{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module PersistentTodo.Command.Handler
  ( handle
  )
where

import           PersistentTodo.Command         ( Command(..) )
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple     ( Connection )

type HandlerStack m = (MonadIO m, MonadReader m, EnvType m ~ Connection)

handle :: HandlerStack m => Command -> m ()
handle = \case
  Add      place -> undefined
  Complete place -> undefined
  Move start end -> undefined
  Remove place   -> undefined
  Clean          -> undefined
  Wipe           -> undefined
