module Presto.Core.Language.Runtime.Permission where

import Prelude

import Control.Monad.Free (foldFree)
import Data.NaturalTransformation (NaturalTransformation)
import Presto.Core.Language.Runtime.Interpreter (PermissionRunner(..), Runtime(..))
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.Permission (PermissionF(..), PermissionM)

interpretPermissionF :: forall eff. Runtime -> NaturalTransformation PermissionF (AppFlow eff)
interpretPermissionF (Runtime _ (PermissionRunner check _) _) (CheckPermissions permissions nextF) = check permissions >>= (pure <<< nextF)
interpretPermissionF (Runtime _ (PermissionRunner _ take) _) (TakePermissions permissions nextF) = take permissions >>= (pure <<< nextF)

runPermission :: forall eff. Runtime -> NaturalTransformation PermissionM (AppFlow eff)
runPermission runtime = foldFree (interpretPermissionF runtime)
