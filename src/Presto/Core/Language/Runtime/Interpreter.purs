module Presto.Core.Language.Runtime.Interpreter
  ( Runtime(..)
  , UIRunner
  , PermissionCheckRunner
  , PermissionTakeRunner
  , PermissionRunner(..)
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
import Data.NaturalTransformation (NaturalTransformation)
import Data.Tuple (Tuple(..))
import Presto.Core.Language.Runtime.API (APIRunner)
import Presto.Core.Language.Runtime.Permission (PermissionRunner(..))
import Presto.Core.Language.Runtime.Store (InterpreterSt)
import Presto.Core.Language.Runtime.UI (UIRunner)
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


interpret :: forall eff s. Runtime -> NaturalTransformation (FlowMethod s) (InterpreterSt eff)
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

run :: forall eff. Runtime -> NaturalTransformation Flow (InterpreterSt eff)
run runtime = foldFree (\(FlowWrapper g) -> runExists (interpret runtime) g)
