{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PersistentTodo.Task
  ( Status(..)
  , Task'(..)
  , Task
  , setStatus
  )
where

import           Opaleye
import           Data.Int
import           Data.Profunctor.Product        ( p2 )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )

data Status
  = Pending
  | Completed

data Task' t s = Task
  { title :: t
  , status :: s
  }

type Task = Task' String Status
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
