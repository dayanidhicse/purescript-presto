module Engineering.Helpers.Commons where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Engineering.Types.App (AppFlow)
import Presto.Core.Flow (GuiF, run, runUI)
import Presto.Core.Types.API (Header(..), Headers(..), Request(..), URL)
import Presto.Core.Types.App (UI)
import Presto.Core.Types.Language.Interaction (class Interact)

foreign import showUI' :: forall e. Fn2 (String -> Eff (ui :: UI | e) Unit) String (Eff (ui :: UI | e) Unit)
foreign import callAPI' :: forall e. (AffError e) -> (AffSuccess String e) -> NativeRequest -> (Eff e Unit)

type NativeHeader = { field :: String , value :: String}
type NativeHeaders = Array NativeHeader
type AffError e = (Error -> Eff e Unit)
type AffSuccess s e = (s -> Eff e Unit)


newtype NativeRequest = NativeRequest
  { method :: String
  , url :: URL
  , payload :: String
  , headers :: NativeHeaders
  }


mkNativeRequest :: Request -> NativeRequest
mkNativeRequest (Request request@{headers: Headers hs}) = NativeRequest
                                          { method : show request.method
                                            , url: request.url
                                            , payload: request.payload
                                            , headers: mkNativeHeader <$> hs
                                            }

mkNativeHeader :: Header -> NativeHeader
mkNativeHeader (Header field val) = { field: field, value: val}


runUI' :: forall a b e. Interact Error a b => a -> AppFlow e b
runUI' a = ExceptT (Right <$> run uif)
  where uif :: Free GuiF b
        uif = runUI a

