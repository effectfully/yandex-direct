{-# LANGUAGE FlexibleContexts, DeriveGeneric #-}
module YandexDirect.Lib
  ( module YandexDirect.Lib
  , module GHC.Generics
  , module Data.Maybe
  , module Data.Text
  , module Data.Aeson.Types
  , module Control.Monad
  , module Control.Arrow
  , module TextShow
  , module TextShow.Generic
  ) where

import GHC.Generics
import Data.Maybe
import Data.Text (Text)
import Data.Aeson.Types hiding (Result, parse)
import Control.Monad
import Control.Arrow
import TextShow
import TextShow.Generic

import Data.Char
import Data.String as String

newtype PureText = PureText
  { getPureText :: Text
  } deriving (Generic)

instance IsString PureText where
  fromString = PureText . String.fromString
  {-# INLINE fromString #-}

-- TODO: add quotes to all three instances?
instance TextShow PureText where
  showb (PureText t) = fromText t
  {-# INLINE showb #-}

instance ToJSON   PureText where
  toJSON (PureText t) = toJSON t
  {-# INLINE toJSON #-}

instance FromJSON PureText where
  parseJSON = fmap PureText . parseJSON
  {-# INLINE parseJSON #-}

optionsLower :: Options
optionsLower = defaultOptions
  { constructorTagModifier = \(c:s) -> toLower c : s
  }

optionsCamel :: Options
optionsCamel = defaultOptions
  { fieldLabelModifier = dropWhile isLower
  }

optionsUnder :: Options
optionsUnder = defaultOptions
  { constructorTagModifier = tail . dropWhile (/= '_')
  }

optionsCamelOmit :: Options
optionsCamelOmit = optionsCamel
  { omitNothingFields  = True
  }

genericToJSONlower :: (Generic a, GToJSON (Rep a)) => a -> Value
genericToJSONlower = genericToJSON optionsLower

deriveToJSONunder :: (Generic a, GToJSON (Rep a)) => a -> Value
deriveToJSONunder = genericToJSON optionsUnder

deriveToJSONcamelOmit :: (Generic a, GToJSON (Rep a)) => a -> Value
deriveToJSONcamelOmit = genericToJSON optionsCamelOmit

deriveParseJSONcamelOmit :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
deriveParseJSONcamelOmit = genericParseJSON optionsCamelOmit

data YesNoEnum = YES | NO deriving (Generic)

instance ToJSON   YesNoEnum
instance FromJSON YesNoEnum

instance TextShow YesNoEnum where
  showbPrec = genericShowbPrec
  {-# INLINE showbPrec #-}
