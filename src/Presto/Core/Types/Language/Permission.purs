module Presto.Core.Types.Language.Permission where
  
import Prelude

import Control.Monad.Free (Free)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)
import Presto.Core.Utils.Inject (class Inject, inject)

data PermissionF a = CheckPermissions (Array Permission) (PermissionStatus -> a)
                   | TakePermissions (Array Permission) (Array PermissionResponse -> a)

instance functorPermission :: Functor PermissionF where
  map f (CheckPermissions g h) = CheckPermissions g (h >>> f)
  map f (TakePermissions g h) = TakePermissions g (h >>> f)


-- | Checks if permissions granted.
checkPermissions :: forall f. Inject PermissionF f => Array Permission -> Free f PermissionStatus
checkPermissions permissions = inject $ CheckPermissions permissions id

-- | Tries to aquire permissions.
takePermissions :: forall f. Inject PermissionF f => Array Permission -> Free f (Array PermissionResponse)
takePermissions permissions = inject $ TakePermissions permissions id
