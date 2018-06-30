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


class Interpret f g where
  interpretAlgebra :: forall a. f a -> Free g a

instance interpretCoproduct :: (Interpret f h, Interpret g h) => Interpret (Coproduct f g) h where
  interpretAlgebra (Coproduct e) = either interpretAlgebra interpretAlgebra e

interpret :: forall f g a. Interpret f g => Free f a -> Free g a
interpret = foldFree interpretAlgebra

class InterpretWithParam p f g where
  interpretWithParam :: forall a. p -> f a -> Free g a

instance interpretWithParamCoproduct :: (InterpretWithParam p f h, InterpretWithParam p g h) => InterpretWithParam p (Coproduct f g) h where
  interpretWithParam p (Coproduct e) = either (interpretWithParam p) (interpretWithParam p) e

interpretWithParameter :: forall p f g a. InterpretWithParam p f g => p -> Free f a -> Free g a
interpretWithParameter p = foldFree (interpretWithParam p)
