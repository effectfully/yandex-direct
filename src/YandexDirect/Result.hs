{-# LANGUAGE DataKinds, TypeFamilies, DeriveFunctor, DeriveGeneric #-}
module YandexDirect.Result where

import YandexDirect.Lib
import YandexDirect.Operation
import YandexDirect.Entity.Dictionary

newtype Result a = Result
  { result :: a
  } deriving (Functor, Generic)

instance ToJSON   a => ToJSON   (Result a)
instance FromJSON a => FromJSON (Result a)

type family ResultOf m a where
  ResultOf 'Add a               = AddResults
  ResultOf 'Get DictionaryNames = Dictionaries

newtype AddResults = AddResults
  { getAddResults :: [ActionResult]
  } deriving (Generic)

data ActionResult = ActionResult
  { getId       :: !(Maybe Integer)
  , getWarnings :: !(Maybe [ExceptionNotification])
  , getErrors   :: !(Maybe [ExceptionNotification])
  } deriving (Generic)

data ExceptionNotification = ExceptionNotification
  { getCode    :: !Int
  , getMessage :: !Text
  , getDetails :: !(Maybe Text)
  } deriving (Generic)

instance ToJSON AddResults            where
  toJSON = deriveToJSONcamelOmit
instance ToJSON ActionResult          where
  toJSON = deriveToJSONcamelOmit
instance ToJSON ExceptionNotification where
  toJSON = deriveToJSONcamelOmit

instance FromJSON AddResults            where
  parseJSON = deriveParseJSONcamelOmit
instance FromJSON ActionResult          where
  parseJSON = deriveParseJSONcamelOmit
instance FromJSON ExceptionNotification where
  parseJSON = deriveParseJSONcamelOmit

collapseActionResult :: ActionResult -> (Maybe Integer, [ExceptionNotification])
collapseActionResult (ActionResult mid mwarnings merrors) =
  (mid, fromMaybe [] mwarnings ++ fromMaybe [] merrors)

collapseActionResults :: [ActionResult] -> ([Maybe Integer], [ExceptionNotification])
collapseActionResults = foldMap (first (:[]) . collapseActionResult)

data Dictionaries = Dictionaries
  { getGeoRegions          :: !(Maybe [GeoRegionsItem])
  , getSupplySidePlatforms :: !(Maybe [SupplySidePlatformsItem]) -- Just for another example.
  } deriving (Generic)

data GeoRegionsItem = GeoRegionsItem
  { getGeoRegionId   :: !Integer
  , getGeoRegionName :: !Text
  , getGeoRegionType :: !Text
  , getParentId      :: !(Maybe Integer)
  } deriving (Generic)

data SupplySidePlatformsItem = SupplySidePlatformsItem
  { getTitle :: !Text
  } deriving (Generic)

instance ToJSON Dictionaries            where
  toJSON = deriveToJSONcamelOmit
instance ToJSON GeoRegionsItem          where
  toJSON = deriveToJSONcamelOmit
instance ToJSON SupplySidePlatformsItem where
  toJSON = deriveToJSONcamelOmit

instance FromJSON Dictionaries            where
  parseJSON = deriveParseJSONcamelOmit
instance FromJSON GeoRegionsItem          where
  parseJSON = deriveParseJSONcamelOmit
instance FromJSON SupplySidePlatformsItem where
  parseJSON = deriveParseJSONcamelOmit
