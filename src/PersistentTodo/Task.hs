module PersistentTodo.Task
  ( Status(..)
  , Task(..)
  , setStatus
  )
where

data Status
  = Pending
  | Completed

data Task = Task
  { title :: String
  , status :: Status
  }

setStatus :: Status -> Task -> Task
setStatus status task = task { status = status }

