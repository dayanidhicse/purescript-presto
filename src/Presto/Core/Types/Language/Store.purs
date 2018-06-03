module Presto.Core.Types.Language.Store where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe)
import Presto.Core.Types.Language.Storage (Key, class Serializable, serialize, deserialize)
import Presto.Core.Utils.Inject (class Inject, inject)

data Store = LocalStore | InMemoryStore

data StoreF a = Get Store Key (Maybe String -> a)
              | Set Store Key String a

instance functorStoreF :: Functor StoreF where
  map f (Get s k g) = Get s k (g >>> f)
  map f (Set s k v g) = Set s k v (f g)

-- | Gets some string from state by key
getS :: forall f. Inject StoreF f => Key -> Free f (Maybe String)
getS key = inject $ Get InMemoryStore key id

-- | Puts a string value into state using key.
setS :: forall f. Inject StoreF f => Key -> String -> Free f Unit
setS key val = inject $ Set InMemoryStore key val unit

-- | Gets some string from localStorage by key
loadS :: forall f. Inject StoreF f => Key -> Free f (Maybe String)
loadS key = inject $ Get LocalStore key id

-- | Puts a string value into the localStorage using key.
saveS :: forall f. Inject StoreF f => Key -> String -> Free f Unit
saveS key val = inject $ Set LocalStore key val unit

-- | Gets some data from state and deserializes to `s` if possible.
get :: forall f s. Inject StoreF f => Serializable s => Key -> Free f (Maybe s)
get key = do
  res <- getS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the state.
set :: forall f s. Inject StoreF f => Serializable s => Key -> s -> Free f Unit
set key val = setS key (serialize val)

-- | Gets some data from local storage and deserializes to `s` if possible.
load :: forall f s. Inject StoreF f => Serializable s => Key -> Free f (Maybe s)
load key = do
  res <- loadS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the local storage.
save :: forall f s. Inject StoreF f => Serializable s => Key -> s -> Free f Unit
save key val = saveS key (serialize val)
