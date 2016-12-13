# YandexDirect

It's a client library for the [Yandex.Direct API version 5](https://tech.yandex.ru/direct/doc/dg/concepts/overview-docpage/?ncrnd=6298) (Yandex.Direct is a Russian version of Google AdWords). Currently this is mostly a proof of concept as it only allows to get Dictionaries and add Ads, AdExtensions, AdGroups, Campaigns, Keywords and SitelinksSets.

There are two main type classes:

```haskell
class ToJSON a => Entity a where
  entityName :: a -> String

class (Entity (PackItems a), ToJSON a) => Item a where
  type PackItems a
  packItems :: [a] -> PackItems a
```

So we pack items into entities. Example instances:

```haskell
data Ads = Ads { getAds :: [AdAddItem] }

data AdAddItem = ...

instance Entity Ads where
  entityName _ = "ads"

instance Item AdAddItem where
  type PackItems AdAddItem = Ads
  packItems = Ads
```

We use `Servant.Client` for querying the API. The main function is

```haskell
data DirectConfig = DirectConfig
  { getToken :: !Text
  , getLogin :: !(Maybe Text)
<<<<<<< HEAD
  , getHost  :: !String
=======
  , getHost  :: !Text
>>>>>>> origin/master
  }

makePerform :: (FromJSON (ResultOf m a), Entity a)
            => DirectConfig -> Manager -> SMethod m -> a -> ClientM (ResultOf m a)
makePerform (DirectConfig token login host) manager smethod entity = result <$> run where
  proxy = Proxy :: Proxy (DirectAPI a r)
  auth  = Just $ "Bearer " `mappend` token
  lang  = Just "ru"
  oper  = Operation (evalSMethod smethod) entity
<<<<<<< HEAD
  url   = BaseUrl Https host 443 $ "/json/v5/" ++ entityName entity
=======
  url   = BaseUrl Https (unpack host) 443 $ "/json/v5/" ++ entityName entity
>>>>>>> origin/master
  run   = client proxy auth lang login oper manager url
```

`makePerform` returns a result wrapped in the Servant's `ClientM` monad and the type of the result depends on the method invoked and the type of the entity passed. Methods are

```haskell
data Method = Add | Update | Delete | Get
```

`SMethod` is a "singletonized" `Method`:

```haskell
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
```

`ResultOf` is a type family that determines the resulting type of an operation:

```haskell
type family ResultOf m a where
  ResultOf 'Add a               = AddResults
  ResultOf 'Get DictionaryNames = Dictionaries
```

So if you add things, the result has type `AddResults`, and if you pass `SGet` together with some dictionary names to `makePerform`, you get the corresponding `Dictionaries`. Dependent types in action.