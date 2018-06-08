module Engineering.Types.App where


import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Prelude (pure, (<$>))
import Presto.Core.Types.App (LOCAL_STORAGE, NETWORK, STORAGE, UI)
import Presto.Core.Types.Language.Flow (Flow)
import FRP (FRP)
import DOM (DOM)
import Control.Monad.Eff.Ref (REF)

foreign import data TIMER :: Effect

type AppEffects = ( avar :: AVAR
                  , console :: CONSOLE
                  , dom :: DOM
                  , exception :: EXCEPTION
                  , frp :: FRP
                  , ls :: LOCAL_STORAGE
                  , network :: NETWORK
                  , ref :: REF
                  , storage :: STORAGE
                  , ui :: UI
                  )

type AppFlow e a = (ExceptT e Flow a)
                  

liftLeft :: forall e b. e -> AppFlow e b
liftLeft e = ExceptT (Left <$> pure e)




