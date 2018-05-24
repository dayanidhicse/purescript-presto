module Presto.Core.Language.Runtime.Store where

import Prelude

import Control.Monad.Aff.AVar (AVar, putVar, readVar, takeVar)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (lift)
import Data.NaturalTransformation (NaturalTransformation)
import Data.StrMap (StrMap, insert, lookup)
import Presto.Core.LocalStorage (getValueFromLocalStore, setValueToLocalStore)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.Storage (Key)
import Presto.Core.Types.Language.Store (Store(..), StoreF(..), StoreM)

type St = AVar (StrMap String)
type InterpreterSt eff a = S.StateT St (AppFlow eff) a

readState :: forall eff. InterpreterSt eff (StrMap String)
readState = S.get >>= (lift <<< readVar)

updateState :: forall eff. Key -> String -> InterpreterSt eff Unit
updateState key value = do
  stVar <- S.get
  st <- lift $ takeVar stVar
  let st' = insert key value st
  lift $ putVar st' stVar

interpretStoreF :: forall eff. NaturalTransformation StoreF (InterpreterSt eff)
interpretStoreF (Get LocalStore key next) = lift $ getValueFromLocalStore key >>= (pure <<< next)
interpretStoreF (Set LocalStore key value next) = do
    lift $ setValueToLocalStore key value
    pure next
interpretStoreF (Get InMemoryStore key next) = readState >>= (lookup key >>> next >>> pure)
interpretStoreF (Set InMemoryStore key value next) = updateState key value *> pure next

runStoreM :: forall eff. NaturalTransformation StoreM (InterpreterSt eff)
runStoreM = foldFree interpretStoreF
