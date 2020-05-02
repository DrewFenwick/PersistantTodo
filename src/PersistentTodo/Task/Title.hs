{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PersistentTodo.Task.Title
  ( Title(..)
  )
where

import           Opaleye
import           Data.Profunctor
import           Data.Profunctor.Product.Default
                                                ( Default(..) )

newtype Title = Title { getTitle :: String} deriving (Show)

instance Default ToFields Title (Column PGText) where
  def = lmap getTitle def

instance QueryRunnerColumnDefault PGText Title where
  queryRunnerColumnDefault = Title <$> fieldQueryRunnerColumn
