module Presto.Core.Language.Runtime.API
  ( APIRunner
  , runAPIInteraction
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError, runExcept)
import Control.Monad.Free (foldFree)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists)
import Data.Foreign.Class (encode, decode)
import Data.NaturalTransformation (NaturalTransformation)
import Data.Tuple (Tuple)
import Presto.Core.Types.API (Request) as API
import Presto.Core.Types.Language.API (APIResult, ApiF(..), ApiMethodF(..), callAPI)
import Presto.Core.Types.Language.Interaction (InteractionF(..), Interaction, ForeignIn(..), ForeignOut(..))
import Presto.Core.Types.Language.Pairing (class Pairing, pair)
import Unsafe.Coerce (unsafeCoerce)


newtype CoApiMethodF a s = CoApiMethodF {
  callAPI :: Interaction (APIResult s) -> Tuple (APIResult s) a
  }

newtype CoApiF a = CoApiF (Exists (CoApiMethodF a))

instance functorCoApiF :: Functor CoApiF where
  map f (CoApiF eo) = CoApiF $ mkExists $ g $ unsafeCoerce eo
    where 
      g (CoApiMethodF co) = CoApiMethodF {
        callAPI: co.callAPI >>> rmap f
      }

instance pairingApi :: Pairing CoApiF ApiF where
  pair p (CoApiF eo) (ApiF ao) = pair p (co.callAPI req) next
    where CoApiMethodF co = unsafeCoerce eo
          CallAPI req next = unsafeCoerce ao

type CoAPI = Cofree CoApiF

type APIRunner = forall e. API.Request -> Aff e String

interpretAPI :: forall eff. APIRunner -> NaturalTransformation InteractionF (Aff eff)
interpretAPI apiRunner (Request (ForeignIn fgnIn) nextF) = do
  case runExcept $ decode fgnIn of
    -- This error should never happen if the `apiInteract` function is made right.
    Left err -> throwError (error ("apiInteract is broken: " <> show err))
    Right req -> do
      str <- apiRunner req
      pure $ nextF $ ForeignOut $ encode str


runAPIInteraction :: forall eff. APIRunner -> NaturalTransformation Interaction (Aff eff)
runAPIInteraction apiRunner = foldFree (interpretAPI apiRunner)
