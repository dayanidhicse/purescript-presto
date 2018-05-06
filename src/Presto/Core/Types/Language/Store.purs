module Presto.Core.Types.Language.Store where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe)
import Presto.Core.Types.Language.Storage (Key, class Serializable, serialize, deserialize)

data Store = LocalStore | InMemoryStore

data StoreF a = Get Store Key (Maybe String -> a)
              | Set Store Key String a

instance functorStoreF :: Functor StoreF where
  map f (Get s k g) = Get s k (g >>> f)
  map f (Set s k v g) = Set s k v (f g)

type StoreM = Free StoreF

-- | Gets some string from state by key
getS :: Key -> StoreM (Maybe String)
getS key = liftF $ Get InMemoryStore key id

-- | Puts a string value into state using key.
setS :: Key -> String -> StoreM Unit
setS key val = liftF $ Set InMemoryStore key val unit

-- | Gets some string from localStorage by key
loadS :: Key -> StoreM (Maybe String)
loadS key = liftF $ Get LocalStore key id

-- | Puts a string value into the localStorage using key.
saveS :: Key -> String -> StoreM Unit
saveS key val = liftF $ Set LocalStore key val unit

-- | Gets some data from state and deserializes to `s` if possible.
get :: forall s. Serializable s => Key -> StoreM (Maybe s)
get key = do
  res <- getS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the state.
set :: forall s. Serializable s => Key -> s -> StoreM Unit
set key val = setS key (serialize val)

-- | Gets some data from local storage and deserializes to `s` if possible.
load :: forall s. Serializable s => Key -> StoreM (Maybe s)
load key = do
  res <- loadS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the local storage.
save :: forall s. Serializable s => Key -> s -> StoreM Unit
save key val = saveS key (serialize val)
