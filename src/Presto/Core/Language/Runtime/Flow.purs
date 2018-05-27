module Presto.Core.Language.Runtime.Flow
  ( Runtime(..)
  , run
  ) where

import Prelude

import Control.Monad.Aff (Aff, Error, delay, forkAff)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, readVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parOneOf)
import Data.Exists (runExists)
import Data.Tuple (Tuple(..))
import Presto.Core.Language.Runtime.Interaction (APIRunner, UIRunner)
import Presto.Core.Language.Runtime.Permission (PermissionRunner(..))
import Presto.Core.Language.Runtime.Store (InterpreterSt)
import Presto.Core.Types.App (STORAGE, UI)
import Presto.Core.Types.Language.Flow (Control(..), Flow, FlowMethodF(..), FlowWrapper(..), FlowMethod)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus)

type AffError e = (Error -> Eff e Unit)
type AffSuccess s e = (s -> Eff e Unit)

data Runtime = Runtime UIRunner PermissionRunner APIRunner

-- TODO: canceller support
forkFlow :: forall eff a. Runtime -> Flow a -> InterpreterSt eff (Control a)
forkFlow rt flow = do
  st <- S.get
  resultVar <- lift makeEmptyVar
  let m = S.evalStateT (run rt flow) st
  _ <- lift $ forkAff $ m >>= flip putVar resultVar
  pure $ Control resultVar


interpret :: forall eff s. Runtime -> FlowMethod s ~> InterpreterSt eff
interpret r (Fork flow nextF) = forkFlow r flow >>= (pure <<< nextF)

interpret _ (DoAff aff nextF) = lift aff >>= (pure <<< nextF)

interpret _ (Await (Control resultVar) nextF) = do
  lift (readVar resultVar) >>= (pure <<< nextF)

interpret _ (Delay duration next) = lift (delay duration) *> pure next

interpret rt (OneOf flows nextF) = do
  st <- S.get
  Tuple a s <- lift $ parOneOf (parFlow st <$> flows)
  S.put s
  pure $ nextF a
  where
    parFlow st flow = S.runStateT (run rt flow) st

run :: forall eff. Runtime -> Flow ~> InterpreterSt eff
run runtime = foldFree (\(FlowWrapper g) -> runExists (interpret runtime) g)
