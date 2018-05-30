module Presto.Core.Types.Language.Types where

import Prelude

import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (either)
import Data.Functor.Coproduct (Coproduct(..))

type Interpreter f m = f ~> m

liftInterpreter :: forall f m t. Monad m => MonadTrans t => (f ~> m) -> f ~> t m
liftInterpreter r = lift <<< r

interpretCoproduct :: forall f g m. (f ~> m) -> (g ~> m) -> (Coproduct f g) ~> m
interpretCoproduct f g (Coproduct e) = either f g e
