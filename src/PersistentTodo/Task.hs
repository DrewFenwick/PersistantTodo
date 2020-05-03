{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PersistentTodo.Task
  ( Status(..)
  , Task'(..)
  , Task
  , Title(..)
  , TaskField
  , Place(..)
  , NumberedTask
  , NumberedTaskField
  , pTask
  )
where

import           Opaleye
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )

import           PersistentTodo.Task.Place
import           PersistentTodo.Task.Status
import           PersistentTodo.Task.Title

data Task' t s = Task
  { title :: t
  , status :: s
  } deriving (Show)

type Task = Task' Title Status
type TaskField = Task' (Field PGText) (Field PGBool)
$(makeAdaptorAndInstance "pTask" ''Task')

type NumberedTask = (Place, Task)
type NumberedTaskField = (Field PGInt4, TaskField)

