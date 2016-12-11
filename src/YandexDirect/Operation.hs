{-# LANGUAGE GADTs, DataKinds, DeriveGeneric #-}
module YandexDirect.Operation where

import YandexDirect.Lib

data Method = Add | Update | Delete | Get
  deriving (Generic)

data SMethod m where
  SAdd    :: SMethod 'Add
  SUpdate :: SMethod 'Update
  SDelete :: SMethod 'Delete
  SGet    :: SMethod 'Get

evalSMethod :: SMethod m -> Method
evalSMethod SAdd    = Add
evalSMethod SUpdate = Update
evalSMethod SDelete = Delete
evalSMethod SGet    = Get

data Operation a = Operation
  { method :: !Method
  , params :: !a
  } deriving (Generic)

instance ToJSON Method where
  toJSON = genericToJSONlower

instance ToJSON a => ToJSON (Operation a)
