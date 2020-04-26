{-# LANGUAGE TypeFamilies #-}

module PersistentTodo.Command.Handler
  ( handle
  )
where

import           PersistentTodo.Command         ( Command )
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple     ( Connection )

handle
  :: (MonadIO m, MonadReader m, EnvType m ~ Connection)
  => Command
  -> m Connection
handle = undefined
