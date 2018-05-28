module Presto.Core.Types.Language.Types where

import Prelude

import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (either)
import Data.Functor.Coproduct (Coproduct(..))

type Interpreter f m = f ~> m

interpretTrans :: forall f m t. Monad m => MonadTrans t => Interpreter f m -> Interpreter f (t m)
interpretTrans interpret = lift <<< interpret

interpretCoproduct :: forall f g m. Interpreter f m -> Interpreter g m -> Interpreter (Coproduct f g) m
interpretCoproduct interpretF interpretG (Coproduct e) = either interpretF interpretG e
