module Presto.Core.Types.Language.API where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Either (Either)
import Data.Exists (Exists, mkExists)
import Data.Foreign.Class (class Decode, class Encode)
import Presto.Core.Types.API (class RestEndpoint, ErrorResponse, Headers)
import Presto.Core.Types.Language.APIInteract (apiInteract)
import Presto.Core.Types.Language.Interaction (Interaction)
import Unsafe.Coerce (unsafeCoerce)

type APIResult s = Either ErrorResponse s

data ApiMethodF a s = CallAPI (Interaction (APIResult s)) (APIResult s -> a)

newtype ApiF a = ApiF (Exists (ApiMethodF a))

instance functorApiF :: Functor ApiF where
  map f (ApiF g) = ApiF $ mkExists $ map' f (unsafeCoerce g)
    where map' f (CallAPI g h) = CallAPI g (h >>> f)

type API = Free ApiF

-- | Call API being authorized.
callAPI :: forall a b. Encode a => Decode b => RestEndpoint a b
  => Headers -> a -> API (APIResult b)
callAPI headers a = liftF $ ApiF $ mkExists $ CallAPI (apiInteract a headers) id
