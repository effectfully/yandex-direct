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
import Control.Monad.Trans.State.Strict

-- See https://mail.haskell.org/pipermail/libraries/2013-March/019528.html
type WriterT = StateT

writerT :: (Monad m, Monoid w) => m (a, w) -> WriterT w m a
writerT p = StateT $ \w' -> second (`mappend` w') <$!> p
{-# INLINE writerT #-}

runWriterT :: Monoid w => WriterT w m a -> m (a, w)
runWriterT w = runStateT w mempty
{-# INLINE runWriterT #-}

tell :: (Monad m, Monoid w) => w -> WriterT w m ()
tell w = StateT $ \w' -> return . (,) () $! w `mappend` w'
{-# INLINE tell #-}

newtype PureText = PureText
  { getPureText :: Text
  } deriving (Generic)

instance IsString PureText where
  fromString = PureText . String.fromString
  {-# INLINE fromString #-}
  
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
