{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module PersistentTodo.Task
  ( Status(..)
  , Task'(..)
  , Task
  , Title(..)
  , setStatus
  , taskInsert
  )
where

import           Opaleye
import           Data.Bool
import           Data.Int
import           Data.Profunctor
import           Data.Profunctor.Product        ( p2 )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import           Data.Profunctor.Product.Default
                                                ( Default(..) )

newtype Title = Title { getTitle :: String}

instance Default ToFields Title (Column PGText) where
  def = lmap getTitle def

data Status
  = Pending
  | Completed

isComplete :: Status -> Bool
isComplete = \case
  Completed -> True
  _         -> False

toStatus :: Bool -> Status
toStatus = bool Pending Completed

instance Default ToFields Status (Column PGBool) where
  def = lmap isComplete def

instance QueryRunnerColumnDefault PGText Title where
  queryRunnerColumnDefault = Title <$> fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PGBool Status where
  queryRunnerColumnDefault = toStatus <$> fieldQueryRunnerColumn

data Task' t s = Task
  { title :: t
  , status :: s
  }

type Task = Task' Title Status
type TaskField = Task' (Field SqlText) (Field SqlBool)
$(makeAdaptorAndInstance "pTask" ''Task')

setStatus :: Status -> Task -> Task
setStatus status task = task { status = status }

taskTable :: Table (Maybe (Field SqlInt4), TaskField) (Field SqlInt4, TaskField)
taskTable = table "taskTable" $ p2
  ( tableField "Id"
  , pTask Task { title = tableField "title", status = tableField "condition" }
  )

taskSelect :: Select (Column SqlInt4, TaskField)
taskSelect = selectTable taskTable

taskInsert :: Maybe Int -> Task -> Insert Int64
taskInsert key task = Insert { iTable      = taskTable
                             , iRows       = pure $ toFields (key, task)
                             , iReturning  = rCount
                             , iOnConflict = Nothing
                             }
