module Presto.Core.Language.Runtime.Store where

import Prelude

import Control.Monad.Free (foldFree)
import Presto.Core.LocalStorage (getValueFromLocalStore, setValueToLocalStore)
import Presto.Core.Types.Language.Flow (Flow, doAff, get, set)
import Presto.Core.Types.Language.Store (Store(..), StoreF(..), StoreM)

interpretStoreF :: StoreF ~> Flow
interpretStoreF (Get LocalStore key next) = doAff (getValueFromLocalStore key) >>= (next >>> pure)
interpretStoreF (Set LocalStore key value next) = doAff (setValueToLocalStore key value) *> pure next
interpretStoreF (Get InMemoryStore key next) = get key >>= (next >>> pure)
interpretStoreF (Set InMemoryStore key value next) = set key value *> pure next

runStoreM :: StoreM ~> Flow
runStoreM = foldFree interpretStoreF
