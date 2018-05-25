module Presto.Core.Language.Runtime.API where

import Prelude

import Control.Monad.Free (foldFree)
import Data.Exists (runExists)
import Data.NaturalTransformation (NaturalTransformation)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.API (ApiF(..), ApiMethodF(..), API)
import Presto.Core.Language.Runtime.Interaction (APIRunner, runAPIInteraction)

type ApiMethodFFlip s a = ApiMethodF a s

interpretApiF :: forall s eff. APIRunner -> NaturalTransformation (ApiMethodFFlip s) (AppFlow eff)
interpretApiF apiRunner (CallAPI apiInteractionF nextF) = runAPIInteraction apiRunner apiInteractionF >>= (pure <<< nextF)

runApi :: forall eff. APIRunner -> NaturalTransformation API (AppFlow eff)
runApi apiRunner = foldFree (\(ApiF a) -> runExists (interpretApiF apiRunner) a)
