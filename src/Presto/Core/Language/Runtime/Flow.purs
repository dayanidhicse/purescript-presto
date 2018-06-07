module Presto.Core.Language.Runtime.Flow
  ( runFlow
  ) where

import Prelude

import Control.Monad.Aff (delay, forkAff)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, readVar, takeVar)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parOneOf)
import Data.StrMap (StrMap, insert, lookup)
import Data.Tuple (Tuple(..))
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.Flow (Control(..), Flow, FlowMethod(..), FlowWrapper(..), Runtime(..), unFlow)
import Presto.Core.Types.Language.Storage (Key)
import Presto.Core.Utils.Existing (runExisting)

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

-- TODO: canceller support
forkFlow :: forall eff a. Runtime -> Flow a -> InterpreterSt eff (Control a)
forkFlow rt flow = do
  s <- S.get
  resultVar <- lift makeEmptyVar
  let m = S.evalStateT (runFlow rt flow) s
  _ <- lift $ forkAff $ m >>= flip putVar resultVar
  pure $ Control resultVar

parFlows :: forall eff a. Runtime -> Array (Flow a) -> InterpreterSt eff a
parFlows rt flows = do
  st <- S.get
  let parFlow state flow = S.runStateT (runFlow rt flow) state
  Tuple a s <- lift $ parOneOf (parFlow st <$> flows)
  S.put s
  pure a

interpretFlow :: forall eff s. Runtime -> FlowMethod s ~> InterpreterSt eff
interpretFlow rt (Fork flow nextF) = forkFlow rt flow >>= (nextF >>> pure)
interpretFlow _ (DoAff aff nextF) = lift aff >>= (nextF >>> pure)
interpretFlow _ (Await (Control resultVar) nextF) = lift (readVar resultVar) >>= (nextF >>> pure)
interpretFlow _ (Delay duration next) = lift (delay duration) *> pure next
interpretFlow rt (OneOf flows nextF) = parFlows rt flows >>= (nextF >>> pure)
interpretFlow (Runtime uiRunner _ _) (UIFlow f nextF) = lift (f uiRunner) >>= (nextF >>> pure)
interpretFlow (Runtime _ permissionRunner _) (PermissionFlow f nextF) = lift (f permissionRunner) >>= (nextF >>> pure)
interpretFlow (Runtime _ _ apiRunner) (APIFlow f nextF) = lift (f apiRunner) >>= (nextF >>> pure)
interpretFlow _ (Set k v next) = updateState k v *> pure next
interpretFlow _ (Get k nextF) = readState >>= (lookup k >>> nextF >>> pure)

runFlow :: forall eff. Runtime -> Flow ~> InterpreterSt eff
runFlow rt = foldFree (\(FlowWrapper g) -> runExisting (interpretFlow rt) g) <<< unFlow
