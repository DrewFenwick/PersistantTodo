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

taskTable :: Table TaskField TaskField
taskTable = table
  "taskTable"
  (pTask Task { title = tableField "title", status = tableField "status" })

taskSelect :: Select TaskField
taskSelect = selectTable taskTable
