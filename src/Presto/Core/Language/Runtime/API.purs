module Presto.Core.Language.Runtime.API where

import Prelude

import Control.Monad.Free (foldFree)
import Data.Exists (runExists)
import Presto.Core.Language.Runtime.Interaction (APIRunner, runAPIInteraction)
import Presto.Core.Types.Language.API (ApiF(..), ApiMethodF(..), API)
import Presto.Core.Types.Language.Flow (Flow, doAff)

type ApiMethodFFlip s a = ApiMethodF a s

interpretApiF :: forall s. APIRunner -> ApiMethodFFlip s ~> Flow
interpretApiF apiRunner (CallAPI apiInteractionF nextF) = doAff (runAPIInteraction apiRunner apiInteractionF) >>= (pure <<< nextF)

runApi :: APIRunner -> API ~> Flow
runApi apiRunner = foldFree (\(ApiF a) -> runExists (interpretApiF apiRunner) a)
