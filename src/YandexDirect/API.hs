{-# LANGUAGE FlexibleContexts, RankNTypes, DataKinds, TypeOperators, OverloadedStrings  #-}
module YandexDirect.API where

import YandexDirect.Lib
import YandexDirect.Operation
import YandexDirect.Entity
import YandexDirect.Result

import System.Random
import Data.Proxy
import Data.DList
import Data.Text (unpack)
import Control.Monad.Trans
import Control.Monad.Writer
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client

-- Currently there is no "; charset=utf-8" on Windows.
type DirectAPI a r = Header "Authorization"   Text
                  :> Header "Accept-Language" Text
                  :> Header "Client-Login"    Text
                  :> ReqBody '[JSON] (Operation a)
                  :> Post    '[JSON] (Result    r)

data DirectConfig = DirectConfig
  { getToken :: !Text
  , getLogin :: !(Maybe Text)
  , getHost  :: !Text
  }

-- We could prove this `FromJSON (ResultOf m a)` using ideas described in
--   http://comonad.com/reader/2011/what-constraints-entail-part-1/
-- but it's tedious.
makePerform :: (FromJSON (ResultOf m a), Entity a)
            => DirectConfig -> Manager -> SMethod m -> a -> ClientM (ResultOf m a)
makePerform (DirectConfig token login host) manager smethod entity = result <$> run where
  proxy = Proxy :: Proxy (DirectAPI a r)
  auth  = Just $ "Bearer " `mappend` token
  lang  = Just "ru"
  oper  = Operation (evalSMethod smethod) entity
  url   = BaseUrl Https (unpack host) 443 $ "/json/v5/" ++ entityName entity
  run   = client proxy auth lang login oper manager url

type ReceiveList a = WriterT (DList ExceptionNotification) ClientM [Maybe a]

type AddItems = forall a. Item a => [a] -> ReceiveList Integer

newtype WrapAddItems = WrapAddItems AddItems

makeDirectAdd :: DirectConfig -> Manager -> WrapAddItems
makeDirectAdd config manager = WrapAddItems add where
  perfSAdd xs = makePerform config manager SAdd $ packItems xs
  add1     xs = second fromList . collapseActionResults . getAddResults <$> perfSAdd xs
  add      xs = if null xs then return [] else WriterT $ add1 xs

emulateAdd :: WrapAddItems
emulateAdd = WrapAddItems $ liftIO . mapM (\_ -> Just <$> randomIO)

associateHandle :: ([ExceptionNotification] -> ClientM ())
                -> (a -> b -> c) -> [a] -> ReceiveList b -> ClientM [c]
associateHandle handle f xs c = do
  (mys, es) <- runWriterT c
  when (not $ null es) . handle $ toList es
  return . catMaybes $ zipWith (fmap . f) xs mys
