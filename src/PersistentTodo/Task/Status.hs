{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PersistentTodo.Task.Status
  ( Status(..)
  )
where

import           Data.Bool
import           Data.Profunctor
import           Data.Profunctor.Product.Default
                                                ( Default(..) )
import           Opaleye

data Status
  = Pending
  | Completed
  deriving (Show)

isComplete :: Status -> Bool
isComplete = \case
  Completed -> True
  _         -> False

toStatus :: Bool -> Status
toStatus = bool Pending Completed

instance Default ToFields Status (Column PGBool) where
  def = lmap isComplete def

instance QueryRunnerColumnDefault PGBool Status where
  queryRunnerColumnDefault = toStatus <$> fieldQueryRunnerColumn
