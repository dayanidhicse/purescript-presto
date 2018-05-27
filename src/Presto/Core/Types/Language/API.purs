module Presto.Core.Types.Language.API where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Either (Either)
import Data.Foreign.Class (class Decode, class Encode)
import Presto.Core.Types.API (class RestEndpoint, ErrorResponse, Headers)
import Presto.Core.Types.Language.APIInteract (apiInteract)
import Presto.Core.Types.Language.Interaction (Interaction)
import Presto.Core.Utils.Existing (Existing, mkExisting, unExisting)

type APIResult s = Either ErrorResponse s

data ApiMethod s a = CallAPI (Interaction (APIResult s)) (APIResult s -> a)

instance functorApiMethod :: Functor (ApiMethod s) where
  map f (CallAPI g h) = CallAPI g (h >>> f)

newtype ApiF a = ApiF (Existing ApiMethod a)

instance functorApiF :: Functor ApiF where
  map f (ApiF g) = ApiF $ mkExisting $ f <$> unExisting g

type API = Free ApiF

-- | Call API being authorized.
callAPI :: forall a b. Encode a => Decode b => RestEndpoint a b
  => Headers -> a -> API (APIResult b)
callAPI headers a = liftF $ ApiF $ mkExisting $ CallAPI (apiInteract a headers) id
