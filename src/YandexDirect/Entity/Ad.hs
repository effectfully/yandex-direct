{-# LANGUAGE TypeFamilies, DeriveGeneric, DuplicateRecordFields #-}
module YandexDirect.Entity.Ad where

import YandexDirect.Lib
import YandexDirect.Entity.Core

instance Entity Ads where
  entityName _ = "ads"

instance Item AdAddItem where
  type PackItems AdAddItem = Ads
  packItems = Ads

newtype Ads = Ads
  { getAds :: [AdAddItem]
  } deriving (Generic)

data AdAddItem = AdAddItem
  { getAdGroupId :: !Integer
  , getTextAd    :: !TextAdAdd
  } deriving (Generic)

data TextAdAdd = TextAdAdd
  { getTitle          :: !PureText
  , getText           :: !PureText
  , getMobile         :: !YesNoEnum
  , getHref           :: !PureText
  , getSitelinkSetId  :: !(Maybe Integer)
  , getAdExtensionIds :: !(Maybe [Integer])
  } deriving (Generic)

instance ToJSON Ads       where
  toJSON = deriveToJSONcamelOmit
instance ToJSON AdAddItem where
  toJSON = deriveToJSONcamelOmit
instance ToJSON TextAdAdd where
  toJSON = deriveToJSONcamelOmit

instance TextShow Ads       where
  showbPrec = genericShowbPrec
instance TextShow AdAddItem where
  showbPrec = genericShowbPrec
instance TextShow TextAdAdd where
  showbPrec = genericShowbPrec
