{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module PersistentTodo.Command.Handler
  ( handle
  )
where

import           PersistentTodo.Command         ( Command(..) )
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple     ( Connection )

handle
  :: (MonadIO m, MonadReader m, EnvType m ~ Connection)
  => Command
  -> m ()
handle = \case
  Add      place -> undefined
  Complete place -> undefined
  Move start end -> undefined
  Remove place   -> undefined
  Clean          -> undefined
  Wipe           -> undefined
