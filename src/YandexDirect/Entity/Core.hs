{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module YandexDirect.Entity.Core where

import YandexDirect.Lib

class ToJSON a => Entity a where
  entityName :: a -> String

class (Entity (PackItems a), ToJSON a) => Item a where
  type PackItems a
  packItems :: [a] -> PackItems a
