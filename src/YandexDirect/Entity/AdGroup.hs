{-# LANGUAGE TypeFamilies, DeriveGeneric, DuplicateRecordFields #-}
module YandexDirect.Entity.AdGroup where

import YandexDirect.Lib
import YandexDirect.Entity.Core

instance Entity AdGroups where
  entityName _ = "adgroups"

instance Item AdGroupAddItem where
  type PackItems AdGroupAddItem = AdGroups
  packItems = AdGroups

newtype AdGroups = AdGroups
  { getAdGroups :: [AdGroupAddItem]
  } deriving (Generic)

data AdGroupAddItem = AdGroupAddItem
  { getName       :: !PureText
  , getCampaignId :: !Integer
  , getRegionIds  :: [Integer]
  } deriving (Generic)

instance ToJSON AdGroups       where
  toJSON = deriveToJSONcamelOmit
instance ToJSON AdGroupAddItem where
  toJSON = deriveToJSONcamelOmit

instance TextShow AdGroups       where
  showbPrec = genericShowbPrec
instance TextShow AdGroupAddItem where
  showbPrec = genericShowbPrec
