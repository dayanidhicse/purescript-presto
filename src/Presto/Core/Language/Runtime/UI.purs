module Presto.Core.Language.Runtime.UI where

import Prelude

import Control.Monad.Aff (error, forkAff, throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Data.Exists (runExists)
import Data.NaturalTransformation (NaturalTransformation)
import Presto.Core.Language.Runtime.Interaction (runUIInteraction, UIRunner)
import Presto.Core.Language.Runtime.Store (InterpreterSt)
import Presto.Core.Types.Language.UI (ErrorHandler(..), Gui, GuiF(..), GuiMethodF(..))

runErrorHandler :: forall eff s. ErrorHandler s -> InterpreterSt eff s
runErrorHandler (ThrowError msg) = throwError $ error msg
runErrorHandler (ReturnResult res) = pure res

type GuiMethodFF s a = GuiMethodF a s

interpretGuiMethodF :: forall s eff. UIRunner -> NaturalTransformation (GuiMethodFF s) (InterpreterSt eff)
interpretGuiMethodF uiRunner (RunUI uiInteraction nextF) =
    lift $ runUIInteraction uiRunner uiInteraction >>= (pure <<< nextF)
interpretGuiMethodF uiRunner (ForkUI uiInteraction next) = do
    _ <- lift $ forkAff $ runUIInteraction uiRunner uiInteraction
    pure next
interpretGuiMethodF _ (InitUIWithScreen uiFlow nextF) = lift uiFlow >>= (pure <<< nextF)
interpretGuiMethodF _ (InitUI uiFlow nextF) = lift uiFlow >>= (pure <<< nextF)
interpretGuiMethodF _ (RunScreen uiFlow nextF) = lift uiFlow >>= (pure <<< nextF)
interpretGuiMethodF _ (ForkScreen uiFlow nextF) = lift (forkAff uiFlow) *> pure nextF
interpretGuiMethodF uiRunner (HandleError flow nextF) =
    foldFree (\(GuiF g) -> runExists (interpretGuiMethodF uiRunner) g) flow >>= runErrorHandler >>= (pure <<< nextF)

runUI :: forall eff. UIRunner -> NaturalTransformation Gui (InterpreterSt eff)
runUI uiRunner = foldFree (\(GuiF g) -> runExists (interpretGuiMethodF uiRunner) g)