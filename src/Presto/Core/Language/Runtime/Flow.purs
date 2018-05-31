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
import Presto.Core.Types.Language.Flow (Control(..), Flow, FlowMethod(..), FlowWrapper(..))
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
forkFlow :: forall eff a. Flow a -> InterpreterSt eff (Control a)
forkFlow flow = do
  s <- S.get
  resultVar <- lift makeEmptyVar
  let m = S.evalStateT (runFlow flow) s
  _ <- lift $ forkAff $ m >>= flip putVar resultVar
  pure $ Control resultVar

parFlows :: forall eff a. Array (Flow a) -> InterpreterSt eff a
parFlows flows = do
  st <- S.get
  let parFlow state flow = S.runStateT (runFlow flow) state
  Tuple a s <- lift $ parOneOf (parFlow st <$> flows)
  S.put s
  pure a

interpretFlow :: forall eff s. FlowMethod s ~> InterpreterSt eff
interpretFlow (Fork flow nextF) = forkFlow flow >>= (nextF >>> pure)
interpretFlow (DoAff aff nextF) = lift aff >>= (nextF >>> pure)
interpretFlow (Await (Control resultVar) nextF) = lift (readVar resultVar) >>= (nextF >>> pure)
interpretFlow (Delay duration next) = lift (delay duration) *> pure next
interpretFlow (OneOf flows nextF) = parFlows flows >>= (nextF >>> pure)
interpretFlow (Set k v next) = updateState k v *> pure next
interpretFlow (Get k nextF) = readState >>= (lookup k >>> nextF >>> pure)

runFlow :: forall eff. Flow ~> InterpreterSt eff
runFlow = foldFree (\(FlowWrapper g) -> runExisting interpretFlow g)
