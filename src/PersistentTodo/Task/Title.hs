{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PersistentTodo.Task.Title
  ( Title(..)
  )
where

import           Data.Profunctor
import           Data.Profunctor.Product.Default
                                                ( Default(..) )
import           Opaleye

newtype Title = Title { getTitle :: String} deriving (Show)

instance Default ToFields Title (Column PGText) where
  def = lmap getTitle def

instance QueryRunnerColumnDefault PGText Title where
  queryRunnerColumnDefault = Title <$> fieldQueryRunnerColumn
