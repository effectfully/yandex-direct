{-# LANGUAGE TypeFamilies, DeriveGeneric, DuplicateRecordFields #-}
module YandexDirect.Entity.AdExtension where

import YandexDirect.Lib
import YandexDirect.Entity.Core

instance Entity AdExtensions where
  entityName _ = "adextensions"

instance Item AdExtensionAddItem where
  type PackItems AdExtensionAddItem = AdExtensions
  packItems = AdExtensions

newtype AdExtensions = AdExtensions
  { getAdExtensions :: [AdExtensionAddItem]
  } deriving (Generic)

data AdExtensionAddItem = AdExtensionAddItem
  { getCallout :: !(Maybe Callout)
  } deriving (Generic)

data Callout = Callout
  { getCalloutText :: !(Maybe PureText)
  } deriving (Generic)

instance ToJSON AdExtensions       where
  toJSON = deriveToJSONcamelOmit
instance ToJSON AdExtensionAddItem where
  toJSON = deriveToJSONcamelOmit
instance ToJSON Callout            where
  toJSON = deriveToJSONcamelOmit

instance TextShow AdExtensions       where
  showbPrec = genericShowbPrec
instance TextShow AdExtensionAddItem where
  showbPrec = genericShowbPrec
instance TextShow Callout            where
  showbPrec = genericShowbPrec
