{-# LANGUAGE TypeFamilies, DeriveGeneric, DuplicateRecordFields #-}
module YandexDirect.Entity.SitelinksSet where

import YandexDirect.Lib
import YandexDirect.Entity.Core

instance Entity SitelinksSets where
  entityName _ = "sitelinks"

instance Item SitelinksSetAddItem where
  type PackItems SitelinksSetAddItem = SitelinksSets
  packItems = SitelinksSets

newtype SitelinksSets = SitelinksSets
  { getSitelinksSets :: [SitelinksSetAddItem]
  } deriving (Generic)

data SitelinksSetAddItem = SitelinksSetAddItem
  { getSitelinks :: [Sitelink]
  } deriving (Generic)

data Sitelink = Sitelink
  { getTitle       :: !PureText
  , getHref        :: !PureText
  , getDescription :: !(Maybe PureText)
  } deriving (Generic)

instance ToJSON SitelinksSets       where
  toJSON = deriveToJSONcamelOmit
instance ToJSON SitelinksSetAddItem where
  toJSON = deriveToJSONcamelOmit
instance ToJSON Sitelink            where
  toJSON = deriveToJSONcamelOmit

instance TextShow SitelinksSets       where
  showbPrec = genericShowbPrec
instance TextShow SitelinksSetAddItem where
  showbPrec = genericShowbPrec
instance TextShow Sitelink            where
  showbPrec = genericShowbPrec
