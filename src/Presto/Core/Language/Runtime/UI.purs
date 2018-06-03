module Presto.Core.Language.Runtime.UI where

import Prelude

import Control.Monad.Aff (error, throwError)
import Control.Monad.Free (foldFree)
import Presto.Core.Language.Runtime.Interaction (runUIInteraction, UIRunner)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.Flow (Flow, doAff, fork)
import Presto.Core.Types.Language.UI (ErrorHandler(..), Gui, GuiF(..), GuiMethodF(..))
import Presto.Core.Utils.Existing (runExisting)

runErrorHandler :: forall eff s. ErrorHandler s -> AppFlow eff s
runErrorHandler (ThrowError msg) = throwError $ error msg
runErrorHandler (ReturnResult res) = pure res

interpretGuiMethodF :: forall s. UIRunner -> GuiMethodF s ~> Flow
interpretGuiMethodF uiRunner (RunUI uiInteraction nextF) =
    doAff (runUIInteraction uiRunner uiInteraction) >>= (nextF >>> pure)
interpretGuiMethodF uiRunner (ForkUI uiInteraction next) =
    fork (doAff (runUIInteraction uiRunner uiInteraction)) *> pure next
interpretGuiMethodF _ (InitUIWithScreen uiFlow nextF) = doAff uiFlow >>= (nextF >>> pure)
interpretGuiMethodF _ (InitUI uiFlow nextF) = doAff uiFlow >>= (nextF >>> pure)
interpretGuiMethodF _ (RunScreen uiFlow nextF) = doAff uiFlow >>= (nextF >>> pure)
interpretGuiMethodF _ (ForkScreen uiFlow nextF) = fork (doAff uiFlow) *> pure nextF
interpretGuiMethodF uiRunner (HandleError err nextF) = doAff (runErrorHandler err) >>= (nextF >>> pure)

runUI :: UIRunner -> Gui ~> Flow
runUI uiRunner = foldFree (\(GuiF g) -> runExisting (interpretGuiMethodF uiRunner) g)
