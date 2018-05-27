module Presto.Core.Utils.Existing where

import Unsafe.Coerce (unsafeCoerce)


-- A special type hack based on Exists
foreign import data Existing :: (Type -> Type -> Type) -> Type -> Type

mkExisting :: forall f a b. f a b -> Existing f b
mkExisting = unsafeCoerce

unExisting :: forall f a b. Existing f b -> f a b
unExisting = unsafeCoerce

runExisting :: forall f b r. (forall a. f a b -> r) -> Existing f b -> r
runExisting = unsafeCoerce