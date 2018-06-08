module Types where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import FRP (FRP)
import Presto.Core.Types.API (URL)
import Presto.Core.Types.App (LOCAL_STORAGE, NETWORK, STORAGE, UI)

foreign import data TIMER :: Effect

type NativeHeader = { field :: String , value :: String}
type AffStorage e = Aff (storage :: STORAGE | e)
type EffStorage e = Eff (storage :: STORAGE | e)
type AffError e = (Error -> Eff e Unit)
type AffSuccess s e = (s -> Eff e Unit)
type NativeHeaders = Array NativeHeader

type AppEffects = (avar :: AVAR, frp :: FRP, dom :: DOM, ref :: REF, ui :: UI, storage :: STORAGE, ls :: LOCAL_STORAGE, exception :: EXCEPTION, network :: NETWORK, console :: CONSOLE)

newtype NativeRequest = NativeRequest
 { method :: String
 , url :: URL
 , payload :: String
 , headers :: NativeHeaders
 }
