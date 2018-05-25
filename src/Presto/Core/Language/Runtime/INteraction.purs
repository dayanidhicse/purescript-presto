module Presto.Core.Language.Runtime.Interaction where
  
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Free (foldFree)
import Data.Either (Either(..))
import Data.Foreign.Class (decode, encode)
import Data.Foreign.JSON (parseJSON)
import Global.Unsafe (unsafeStringify)
import Presto.Core.Types.API as API
import Presto.Core.Types.App (AppFlow, UI)
import Presto.Core.Types.Language.Interaction (ForeignIn(..), ForeignOut(..), Interaction, InteractionF(..))

type UIRunner = forall e. String -> Aff (ui :: UI | e) String

interpretInteractionF :: forall eff. UIRunner -> InteractionF ~> AppFlow eff
interpretInteractionF uiRunner (Request fgnIn nextF) = do
    json <- uiRunner $ unsafeStringify fgnIn
    case (runExcept (parseJSON json)) of
        Right fgnOut -> pure $ nextF $ ForeignOut fgnOut
        Left err -> throwError $ error $ show err


runUIInteraction :: forall eff. UIRunner -> Interaction ~> AppFlow eff
runUIInteraction uiRunner = foldFree (interpretInteractionF uiRunner)

type APIRunner = forall e. API.Request -> Aff e String

interpretAPIInteractionF :: forall eff. APIRunner -> InteractionF ~> AppFlow eff
interpretAPIInteractionF apiRunner (Request (ForeignIn fgnIn) nextF) = do
    case runExcept $ decode fgnIn of
        -- this error should never happen if the `apiInteract` function is made right
        Left err -> throwError $ error $ "apiInteract is broken: " <> show err
        Right req -> do
            str <- apiRunner req
            pure $ nextF $ ForeignOut $ encode str

runAPIInteraction :: forall eff. APIRunner -> Interaction ~> AppFlow eff
runAPIInteraction apiRunner = foldFree (interpretAPIInteractionF apiRunner)
