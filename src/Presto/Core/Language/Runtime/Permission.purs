module Presto.Core.Language.Runtime.Permission where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Bifunctor (rmap)
import Data.Tuple (Tuple(..))
import Presto.Core.Types.Language.Pairing (class Pairing, pair)
import Presto.Core.Types.Language.Permission (PermissionF(..), checkPermissions, takePermissions)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)

newtype CoPermissionF a = CoPermissionF {
      checkPermission :: Array Permission -> Tuple PermissionStatus a
    , takePermission :: Array Permission -> Tuple (Array PermissionResponse) a
    }

instance functorCoPermissionF :: Functor CoPermissionF where
    map f (CoPermissionF co) = CoPermissionF {
        checkPermission : co.checkPermission >>> (rmap f),
        takePermission : co.takePermission >>> (rmap f)
    }

instance pairingCoPermissionFPermisson :: Pairing CoPermissionF PermissionF where
    pair p (CoPermissionF co) (CheckPermissions a b) = pair p (co.checkPermission a) b
    pair p (CoPermissionF co) (TakePermissions a b) = pair p (co.takePermission a) b

type CoPermission = Cofree CoPermissionF
