module Presto.Core.Utils.Inject where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..), coproduct)
import Data.Maybe (Maybe(..))

class Inject f g where
    inj :: forall a. f a -> g a
    prj :: forall a. g a -> Maybe (f a)

instance injectIdentity :: Inject f f where
    inj = id
    prj = Just

instance injectLeftCoproduct :: Inject f (Coproduct f g) where
    inj = Coproduct <<< Left
    prj = coproduct Just (const Nothing)

instance injectRightCoproduct :: Inject f g => Inject f (Coproduct h g) where
    inj = Coproduct <<< Right <<< inj
    prj = coproduct (const Nothing) prj

inject :: forall f g a. Inject f g => f a -> Free g a
inject f = liftF $ inj f
