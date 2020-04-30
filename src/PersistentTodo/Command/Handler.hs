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
import           PersistentTodo.Task
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple     ( Connection )
import           Opaleye

type HandlerStack m = (MonadIO m, MonadReader m, EnvType m ~ Connection)

handle :: HandlerStack m => Command -> m ()
handle = \case
  Add      title -> add title
  Complete place -> complete place
  Move start end -> move start end
  Remove place   -> remove place
  Clean          -> clean
  Wipe           -> wipe

add :: HandlerStack m => Title -> m ()
add title = do
  conn <- ask
  _ <- liftIO . runInsert_ conn $ taskInsert Nothing (Task title Pending)
  printTasks

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

printTasks :: HandlerStack m => m ()
printTasks = undefined