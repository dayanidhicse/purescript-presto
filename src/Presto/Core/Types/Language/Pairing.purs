module Presto.Core.Types.Language.Pairing where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail)
import Control.Monad.Free (Free, resume)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Product (Product(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..), uncurry)


-- | Pair two functors together
class (Functor f, Functor g) <= Pairing f g where
    pair :: forall a b c. (a -> b -> c) -> f a -> g b -> c


instance pairingIdentity :: Pairing Identity Identity where
    pair p (Identity a) (Identity b) = p a b

instance pairingFuncTuple :: Pairing ((->) a) (Tuple a) where
    pair p f = uncurry (p <<< f)

instance pairingTupleFunc :: Pairing (Tuple a) ((->) a) where
    pair p f g = pair (flip p) g f

-- | pairing a Free and Cofree together
instance pairingFreeCofree :: (Functor f, Functor g, Pairing f g) => Pairing (Cofree f) (Free g) where
    pair p cf f = case resume f of
                    Right a -> p (head cf) a
                    Left f' -> pair (pair p) (tail cf) f'

instance pairingCoProduct :: (Functor g, Functor f, Functor k, Functor h, Pairing g f, Pairing k h) => Pairing (Product g k) (Coproduct f h) where
    pair p (Product (Tuple g _)) (Coproduct (Left f)) = pair p g f
    pair p (Product (Tuple _ k)) (Coproduct (Right h)) = pair p k h