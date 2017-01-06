{-# LANGUAGE TypeFamilies, DeriveGeneric, DuplicateRecordFields #-}
module YandexDirect.Entity.Keyword where

import YandexDirect.Lib
import YandexDirect.Entity.Core

instance Entity Keywords where
  entityName _ = "keywords"

instance Item KeywordAddItem where
  type PackItems KeywordAddItem = Keywords
  packItems = Keywords

newtype Keywords = Keywords
  { getKeywords :: [KeywordAddItem]
  } deriving (Generic)

data KeywordAddItem = KeywordAddItem
  { getAdGroupId :: !Integer
  , getKeyword   :: !PureText
  } deriving (Generic)

instance ToJSON Keywords       where
  toJSON = deriveToJSONcamelOmit
instance ToJSON KeywordAddItem where
  toJSON = deriveToJSONcamelOmit

instance TextShow Keywords       where
  showbPrec = genericShowbPrec
instance TextShow KeywordAddItem where
  showbPrec = genericShowbPrec
