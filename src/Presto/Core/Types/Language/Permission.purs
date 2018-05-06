module Presto.Core.Types.Language.Permission where
  
import Prelude

import Control.Monad.Free (Free, liftF)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)

data PermissionF a = CheckPermissions (Array Permission) (PermissionStatus -> a)
                   | TakePermissions (Array Permission) (Array PermissionResponse -> a)

instance functorPermission :: Functor PermissionF where
  map f (CheckPermissions g h) = CheckPermissions g (h >>> f)
  map f (TakePermissions g h) = TakePermissions g (h >>> f)

type PermissionM = Free PermissionF

-- | Checks if permissions granted.
checkPermissions :: Array Permission -> PermissionM PermissionStatus
checkPermissions permissions = liftF $ CheckPermissions permissions id

-- | Tries to aquire permissions.
takePermissions :: Array Permission -> PermissionM (Array PermissionResponse)
takePermissions permissions = liftF $ TakePermissions permissions id
