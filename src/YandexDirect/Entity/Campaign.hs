{-# LANGUAGE TypeFamilies, DeriveGeneric, DuplicateRecordFields #-}
module YandexDirect.Entity.Campaign where

import YandexDirect.Lib
import YandexDirect.Entity.Core

instance Entity Campaigns where
  entityName _ = "campaigns"

instance Item CampaignAddItem where
  type PackItems CampaignAddItem = Campaigns
  packItems = Campaigns

newtype Campaigns = Campaigns
  { getCampaigns :: [CampaignAddItem]
  } deriving (Generic)

data CampaignAddItem = CampaignAddItem
  { getName         :: !PureText
  , getStartDate    :: !PureText
  , getTextCampaign :: !TextCampaignAddItem
  } deriving (Generic)

data TextCampaignAddItem = TextCampaignAddItem
  { getBiddingStrategy :: !TextCampaignStrategyAdd
  } deriving (Generic)

data TextCampaignStrategyAdd = TextCampaignStrategyAdd
  { getSearch  :: !TextCampaignSearchStrategyAdd
  , getNetwork :: !TextCampaignNetworkStrategyAdd
  } deriving (Generic)

data TextCampaignSearchStrategyAdd = TextCampaignSearchStrategyAdd
  { getBiddingStrategyType :: !TextCampaignSearchStrategyTypeEnum
  } deriving (Generic)

data TextCampaignNetworkStrategyAdd = TextCampaignNetworkStrategyAdd
  { getBiddingStrategyType :: !TextCampaignNetworkStrategyTypeEnum
  } deriving (Generic)

data TextCampaignSearchStrategyTypeEnum
  = SEARCH_LOWEST_COST
  | SEARCH_SERVING_OFF
  deriving (Generic)

data TextCampaignNetworkStrategyTypeEnum
  = NETWORK_NETWORK_DEFAULT
  | NETWORK_MAXIMUM_COVERAGE
  | NETWORK_SERVING_OFF
  deriving (Generic)

instance ToJSON Campaigns                           where
  toJSON = deriveToJSONcamelOmit
instance ToJSON CampaignAddItem                     where
  toJSON = deriveToJSONcamelOmit
instance ToJSON TextCampaignAddItem                 where
  toJSON = deriveToJSONcamelOmit
instance ToJSON TextCampaignStrategyAdd             where
  toJSON = deriveToJSONcamelOmit
instance ToJSON TextCampaignSearchStrategyAdd       where
  toJSON = deriveToJSONcamelOmit
instance ToJSON TextCampaignNetworkStrategyAdd      where
  toJSON = deriveToJSONcamelOmit
instance ToJSON TextCampaignSearchStrategyTypeEnum  where
  toJSON = deriveToJSONunder
instance ToJSON TextCampaignNetworkStrategyTypeEnum where
  toJSON = deriveToJSONunder

instance TextShow Campaigns                           where
  showbPrec = genericShowbPrec
instance TextShow CampaignAddItem                     where
  showbPrec = genericShowbPrec
instance TextShow TextCampaignAddItem                 where
  showbPrec = genericShowbPrec
instance TextShow TextCampaignStrategyAdd             where
  showbPrec = genericShowbPrec
instance TextShow TextCampaignSearchStrategyAdd       where
  showbPrec = genericShowbPrec
instance TextShow TextCampaignNetworkStrategyAdd      where
  showbPrec = genericShowbPrec
instance TextShow TextCampaignSearchStrategyTypeEnum  where
  showbPrec = genericShowbPrec
instance TextShow TextCampaignNetworkStrategyTypeEnum where
  showbPrec = genericShowbPrec
