module Presto.Core.Language.Runtime.Permission where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Free (foldFree)
import Data.NaturalTransformation (NaturalTransformation)
import Presto.Core.Types.App (AppFlow, STORAGE)
import Presto.Core.Types.Language.Permission (PermissionF(..), PermissionM)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)

type PermissionCheckRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) PermissionStatus
type PermissionTakeRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) (Array PermissionResponse)
data PermissionRunner = PermissionRunner PermissionCheckRunner PermissionTakeRunner

interpretPermissionF :: forall eff. PermissionRunner -> NaturalTransformation PermissionF (AppFlow eff)
interpretPermissionF (PermissionRunner check _) (CheckPermissions permissions nextF) = check permissions >>= (pure <<< nextF)
interpretPermissionF (PermissionRunner _ take) (TakePermissions permissions nextF) = take permissions >>= (pure <<< nextF)

runPermission :: forall eff. PermissionRunner -> NaturalTransformation PermissionM (AppFlow eff)
runPermission pRunner = foldFree (interpretPermissionF pRunner)
