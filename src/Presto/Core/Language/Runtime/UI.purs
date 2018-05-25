module Presto.Core.Language.Runtime.UI where

import Prelude

import Control.Monad.Aff (error, throwError)
import Control.Monad.Free (foldFree)
import Data.Exists (runExists)
import Presto.Core.Language.Runtime.Interaction (runUIInteraction, UIRunner)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.Flow (Flow, doAff, fork)
import Presto.Core.Types.Language.UI (ErrorHandler(..), Gui, GuiF(..), GuiMethodF(..))

runErrorHandler :: forall eff s. ErrorHandler s -> AppFlow eff s
runErrorHandler (ThrowError msg) = throwError $ error msg
runErrorHandler (ReturnResult res) = pure res

type GuiMethodFF s a = GuiMethodF a s

interpretGuiMethodF :: forall s. UIRunner -> GuiMethodFF s ~> Flow
interpretGuiMethodF uiRunner (RunUI uiInteraction nextF) =
    doAff (runUIInteraction uiRunner uiInteraction) >>= (pure <<< nextF)
interpretGuiMethodF uiRunner (ForkUI uiInteraction next) = do
    _ <- fork $ doAff (runUIInteraction uiRunner uiInteraction)
    pure next
interpretGuiMethodF _ (InitUIWithScreen uiFlow nextF) = doAff uiFlow >>= (pure <<< nextF)
interpretGuiMethodF _ (InitUI uiFlow nextF) = doAff uiFlow >>= (pure <<< nextF)
interpretGuiMethodF _ (RunScreen uiFlow nextF) = doAff uiFlow >>= (pure <<< nextF)
interpretGuiMethodF _ (ForkScreen uiFlow nextF) = do
    _ <- fork $ doAff uiFlow
    pure nextF
interpretGuiMethodF uiRunner (HandleError flow nextF) =
    foldFree (\(GuiF g) -> runExists (interpretGuiMethodF uiRunner) g) flow >>= (doAff <<< runErrorHandler) >>= (pure <<< nextF)

runUI :: UIRunner -> Gui ~> Flow
runUI uiRunner = foldFree (\(GuiF g) -> runExists (interpretGuiMethodF uiRunner) g)
