module Presto.Core.Language.Runtime.API where

import Prelude

import Control.Monad.Free (foldFree)
import Presto.Core.Language.Runtime.Interaction (APIRunner, runAPIInteraction)
import Presto.Core.Types.Language.API (API, ApiF(..), ApiMethod(..))
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Presto.Core.Utils.Existing (runExisting)

interpretApiF :: forall s. APIRunner -> ApiMethod s ~> Flow
interpretApiF apiRunner (CallAPI apiInteractionF nextF) =
    doAff (runAPIInteraction apiRunner apiInteractionF) >>= (nextF >>> pure)

runApi :: APIRunner -> API ~> Flow
runApi apiRunner = foldFree (\(ApiF a) -> runExisting (interpretApiF apiRunner) a)
