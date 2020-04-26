{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module PersistentTodo.Command.Handler
  ( handle
  )
where

import           PersistentTodo.Command         ( Command(..)
                                                , Place
                                                )
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple     ( Connection )

type HandlerStack m = (MonadIO m, MonadReader m, EnvType m ~ Connection)

handle :: HandlerStack m => Command -> m ()
handle = \case
  Add      title -> add title
  Complete place -> complete place
  Move start end -> move start end
  Remove place   -> remove place
  Clean          -> clean
  Wipe           -> wipe

add :: HandlerStack m => String -> m ()
add = undefined

complete :: HandlerStack m => Place -> m ()
complete = undefined

move :: HandlerStack m => Place -> Place -> m ()
move = undefined

remove :: HandlerStack m => Place -> m ()
remove = undefined

clean :: HandlerStack m => m ()
clean = undefined

wipe :: HandlerStack m => m ()
wipe = undefined
