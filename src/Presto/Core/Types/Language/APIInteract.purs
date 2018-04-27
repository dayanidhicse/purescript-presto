module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, class Encode, decode, encode)

import Presto.Core.Types.Language.Interaction (Interaction, request)
import Presto.Core.Types.API (class RestEndpoint, ErrorPayload(..), ErrorResponse, Response(..), Headers, decodeResponse, makeRequest)
import Presto.Core.Utils.Encoding (defaultDecodeJSON)

foreign import unsafeParseJson :: forall a b. b -> {|a}

-- Special interact function for API.
apiInteract :: forall a b.
  Encode a => Decode b => RestEndpoint a b
  => a -> Headers -> Interaction (Either ErrorResponse b)
apiInteract a headers = do
  fgnOut <- request (encode (makeRequest a headers))
  let jsonifiedFgnOut = unsafeParseJson fgnOut
  pure $ case runExcept (decode fgnOut >>= decodeResponse) of
    -- Try to decode the server's resopnse into the expected type
    Right resp -> Right resp
    Left x -> Left $ case runExcept (decode fgnOut >>= defaultDecodeJSON) of
                       -- See if the server sent an error response, else create our own
                       Right e@(Response _) -> e
                       Left y -> Response { code: (jsonifiedFgnOut.code)
                                          , status: ""
                                          , response: ErrorPayload { status : "FAILURE"
                                                                   , responseCode: show x <> "\n" <> show y
                                                                   , responseMessage: "Unknown error"
                                                                   }
                                          }
