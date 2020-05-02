{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module PersistentTodo.Command.Handler
  ( handle
  )
where

import           PersistentTodo.Command         ( Command(..) )
import qualified PersistentTodo.Task           as Task
import           PersistentTodo.Task            ( TaskField
                                                , Task'(Task)
                                                , Task
                                                , taskInsert
                                                , taskSelect
                                                )
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class
import           Data.Foldable
import           Database.PostgreSQL.Simple     ( Connection )
import           Opaleye

type HandlerStack m = (MonadIO m, MonadReader m, EnvType m ~ Connection)

handle :: HandlerStack m => Command -> m ()
handle = \case
  Add title        -> add title
  Set  s     place -> set s place
  Move start end   -> move start end
  Remove place     -> remove place
  Clean            -> clean
  Wipe             -> wipe

add :: HandlerStack m => Task.Title -> m ()
add title = do
  usingConnection runInsert_ $ taskInsert Nothing (Task title Task.Pending)
  printTasks

set :: HandlerStack m => Task.Status -> Task.Place -> m ()
set s (Task.Place place) = do
  _ <- usingConnection runUpdate_ $ Task.setStatus s place
  printTasks

move :: HandlerStack m => Task.Place -> Task.Place -> m ()
move = undefined

remove :: HandlerStack m => Task.Place -> m ()
remove = undefined

clean :: HandlerStack m => m ()
clean = do
  usingConnection runDelete_
    $ Task.deleteTasks ((.== toFields Task.Completed) . Task.status . snd)
  printTasks

wipe :: HandlerStack m => m ()
wipe = undefined

printTasks :: HandlerStack m => m ()
printTasks = do
  tasks <- usingConnection
    (runSelect :: Connection
      -> Select (Column SqlInt4, TaskField)
      -> IO [(Int, Task)]
    )
    taskSelect
  liftIO $ traverse_ print tasks

usingConnection :: HandlerStack m => (Connection -> a -> IO b) -> a -> m b
usingConnection runner action = liftIO . (`runner` action) =<< ask