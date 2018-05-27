module Presto.Core.Language.Runtime.Flow
  ( runFlow
  ) where

import Prelude

import Control.Monad.Aff (Error, delay, forkAff)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, readVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (foldFree)
import Control.Parallel (parOneOf)
import Data.Exists (runExists)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.Flow (Control(..), Flow, FlowMethodF(..), FlowWrapper(..), FlowMethod)

type AffError e = (Error -> Eff e Unit)
type AffSuccess s e = (s -> Eff e Unit)

-- TODO: canceller support
forkFlow :: forall eff a. Flow a -> AppFlow eff (Control a)
forkFlow flow = do
  resultVar <- makeEmptyVar
  _ <- forkAff $ runFlow flow >>= flip putVar resultVar
  pure $ Control resultVar


interpretFlow :: forall eff s. FlowMethod s ~> AppFlow eff
interpretFlow (Fork flow nextF) = forkFlow flow >>= (nextF >>> pure)
interpretFlow (DoAff aff nextF) = aff >>= (nextF >>> pure)
interpretFlow (Await (Control resultVar) nextF) = readVar resultVar >>= (nextF >>> pure)
interpretFlow (Delay duration next) = delay duration *> pure next
interpretFlow (OneOf flows nextF) = parOneOf (runFlow <$> flows) >>= (nextF >>> pure)

runFlow :: forall eff. Flow ~> AppFlow eff
runFlow = foldFree (\(FlowWrapper g) -> runExists interpretFlow g)
