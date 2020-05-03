{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PersistentTodo.Task.Place
  ( Place(..)
  )
where

import           Data.Profunctor
import           Data.Profunctor.Product.Default
                                                ( Default(..) )
import           Opaleye

newtype Place = Place {getPlace :: Int} deriving (Eq, Ord)

instance Default ToFields Place (Column PGInt4) where
  def = lmap getPlace def

instance QueryRunnerColumnDefault PGInt4 Place where
  queryRunnerColumnDefault = Place <$> fieldQueryRunnerColumn
