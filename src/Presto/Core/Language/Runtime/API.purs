module Presto.Core.Language.Runtime.API where

import Prelude

import Control.Monad.Free (foldFree)
import Data.Exists (runExists)
import Data.NaturalTransformation (NaturalTransformation)
import Presto.Core.Language.Runtime.Interpreter (Runtime(..))
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.API (ApiF(..), ApiMethodF(..), API)

interpretApiF :: forall a eff. Runtime -> NaturalTransformation (ApiMethodF a) (AppFlow eff)
interpretApiF (Runtime _ _ apiRunner) (CallAPI apiInteractionF nextF) =
    runAPIInteraction apiRunner apiInteractionF >>>= (pure <<< nextF)

runApi :: forall eff. Runtime -> NaturalTransformation API (AppFlow eff)
runApi runtime = foldFree (\ApiF a -> runExists (interpretApiF runtime) a)
