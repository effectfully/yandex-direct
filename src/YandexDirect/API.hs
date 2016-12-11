{-# LANGUAGE FlexibleContexts, RankNTypes, DataKinds, TypeOperators, OverloadedStrings  #-}
module YandexDirect.API where

import YandexDirect.Lib
import YandexDirect.Operation
import YandexDirect.Entity
import YandexDirect.Result

import System.Random
import Data.Proxy
import Control.Monad.Trans
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
  , getHost  :: !String
  }

makePerform :: (FromJSON (ResultOf m a), Entity a)
            => DirectConfig -> Manager -> SMethod m -> a -> ClientM (ResultOf m a)
makePerform (DirectConfig token login host) manager smethod entity = result <$> run where
  proxy = Proxy :: Proxy (DirectAPI a r)
  auth  = Just $ "Bearer " `mappend` token
  lang  = Just "ru"
  oper  = Operation (evalSMethod smethod) entity
  url   = BaseUrl Https host 443 $ "/json/v5/" ++ entityName entity
  run   = client proxy auth lang login oper manager url

type ReceiveList a = WriterT [ExceptionNotification] ClientM [Maybe a]

type AddEntity = forall a. Item a => [a] -> ReceiveList Integer
newtype WrapAddEntity = WrapAddEntity AddEntity

makeDirectAdd :: DirectConfig -> Manager -> WrapAddEntity
makeDirectAdd config manager = WrapAddEntity add where
  perfSAdd xs = makePerform config manager SAdd $ packItems xs
  add1     xs = collapseActionResults . getAddResults <$> perfSAdd xs
  add      xs = if null xs then return [] else writerT $ add1 xs

emulateAdd :: WrapAddEntity
emulateAdd = WrapAddEntity $ liftIO . mapM (\_ -> Just <$> randomIO)

associateHandle :: ([ExceptionNotification] -> ClientM ())
                -> (a -> b -> c) -> [a] -> ReceiveList b -> ClientM [c]
associateHandle handle f xs c = do
  (mys, es) <- runWriterT c
  when (not $ null es) $ handle es
  return . catMaybes $ zipWith (fmap . f) xs mys
