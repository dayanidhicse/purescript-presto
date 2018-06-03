module Presto.Core.Types.Language.Types where

import Control.Monad.Free (Free, foldFree)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (either)
import Data.Functor.Coproduct (Coproduct(..))

class Run f m where
    runAlgebra :: forall a. f a -> m a

instance runCoproduct :: (Run f m, Run g m) => Run (Coproduct f g) m where
    runAlgebra (Coproduct e) = either runAlgebra runAlgebra e

run :: forall f m a. Run f m => MonadRec m => Free f a -> m a
run = foldFree runAlgebra
