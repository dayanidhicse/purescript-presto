module Presto.Core.Language.Runtime.Interaction where
  
import Prelude

import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Free (foldFree)
import Data.Either (Either(..))
import Data.Foreign.JSON (parseJSON)
import Data.NaturalTransformation (NaturalTransformation)
import Global.Unsafe (unsafeStringify)
import Presto.Core.Language.Runtime.Interpreter (UIRunner)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.Interaction (ForeignOut(..), InteractionF(..), Interaction)

interpretInteractionF :: forall eff. UIRunner -> NaturalTransformation InteractionF (AppFlow eff)
interpretInteractionF uiRunner (Request fgnIn nextF) = do
    json <- uiRunner $ unsafeStringify fgnIn
    case (runExcept (parseJSON json)) of
        Right fgnOut -> pure $ nextF $ ForeignOut fgnOut
        Left err -> throwError $ error $ show err


runInteraction :: forall eff. UIRunner -> NaturalTransformation Interaction (AppFlow eff)
runInteraction uiRunner = foldFree (interpretInteractionF uiRunner)
