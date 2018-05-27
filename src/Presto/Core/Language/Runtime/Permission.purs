module Presto.Core.Language.Runtime.Permission where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Free (foldFree)
import Presto.Core.Types.App (STORAGE)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Presto.Core.Types.Language.Permission (PermissionF(..), PermissionM)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)

type PermissionCheckRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) PermissionStatus
type PermissionTakeRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) (Array PermissionResponse)
data PermissionRunner = PermissionRunner PermissionCheckRunner PermissionTakeRunner

interpretPermissionF :: PermissionRunner -> PermissionF ~> Flow
interpretPermissionF (PermissionRunner check _) (CheckPermissions permissions nextF) = doAff (check permissions) >>= (nextF >>> pure)
interpretPermissionF (PermissionRunner _ take) (TakePermissions permissions nextF) = doAff (take permissions) >>= (nextF >>> pure)

runPermission :: PermissionRunner -> PermissionM ~> Flow
runPermission pRunner = foldFree (interpretPermissionF pRunner)
