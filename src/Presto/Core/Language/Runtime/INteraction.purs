module Presto.Core.Language.Runtime.Interaction where
  
import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Bifunctor (rmap)
import Data.Tuple (Tuple(..))
import Presto.Core.Types.Language.Interaction (ForeignIn(..), ForeignOut(..), InteractionF(..))
import Presto.Core.Types.Language.Pairing (class Pairing, pair)

data CoInteractionF a = CoRequest (ForeignIn -> Tuple ForeignOut a)

instance functorCoInteractionF :: Functor CoInteractionF where
    map f (CoRequest g) = CoRequest $ g >>> rmap f

instance pairingCoInteraction :: Pairing CoInteractionF InteractionF where
    pair p (CoRequest g) (Request req next) = pair p (g req) next

type CoInteraction = Cofree CoInteractionF
