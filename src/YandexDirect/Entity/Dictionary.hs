{-# LANGUAGE TypeFamilies, DeriveGeneric, DuplicateRecordFields #-}
module YandexDirect.Entity.Dictionary where

import YandexDirect.Lib
import YandexDirect.Entity.Core

instance Entity DictionaryNames where 
  entityName _ = "dictionaries"

instance Item DictionaryNameEnum where
  type PackItems DictionaryNameEnum = DictionaryNames
  packItems = DictionaryNames

data DictionaryNames = DictionaryNames
  { getDictionaryNames :: [DictionaryNameEnum]
  } deriving (Generic)

data DictionaryNameEnum
  = Currencies
  | MetroStations
  | GeoRegions
  | TimeZones
  | Constants
  | AdCategories
  | OperationSystemVersions
  | ProductivityAssertions
  | SupplySidePlatforms
  deriving (Generic)

instance ToJSON DictionaryNames    where
  toJSON = deriveToJSONcamelOmit
instance ToJSON DictionaryNameEnum

instance TextShow DictionaryNames    where
  showbPrec = genericShowbPrec
instance TextShow DictionaryNameEnum where
  showbPrec = genericShowbPrec
