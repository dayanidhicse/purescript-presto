module Presto.Core.Language.Runtime.Store where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Array.ST.Iterator (next)
import Data.Bifunctor (rmap)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Presto.Core.Types.Language.Pairing (class Pairing, pair)
import Presto.Core.Types.Language.Storage (Key)
import Presto.Core.Types.Language.Store (Store, StoreF(..))

newtype CoStoreF a = CoStoreF {
    get :: Store -> Key -> Tuple (Maybe String) a
  , set :: Store -> Key -> String -> a
}

instance functorCoStoreF :: Functor CoStoreF where 
    map f (CoStoreF co) = CoStoreF {
        get: \s k -> rmap f $ co.get s k
      , set: \s k v -> f $ co.set s k v
    }

instance pairingCoStore :: Pairing CoStoreF StoreF where
    pair p (CoStoreF co) (Get s k next) = pair p (co.get s k) next
    pair p (CoStoreF co) (Set s k v next) = p (co.set s k v) next

type CoStore = Cofree CoStoreF
