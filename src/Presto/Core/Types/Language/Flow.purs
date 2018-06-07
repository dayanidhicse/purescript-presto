module Presto.Core.Types.Language.Flow where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Presto.Core.Language.Runtime.Interaction (UIRunner, APIRunner)
import Presto.Core.Types.App (AppFlow, STORAGE)
import Presto.Core.Types.Language.Storage (Key)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)
import Presto.Core.Utils.Existing (Existing, mkExisting, unExisting)

type PermissionCheckRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) PermissionStatus
type PermissionTakeRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) (Array PermissionResponse)
data PermissionRunner = PermissionRunner PermissionCheckRunner PermissionTakeRunner

data Runtime = Runtime UIRunner PermissionRunner APIRunner

newtype Control s = Control (AVar s)

-- | Algebra of the Flow free language.
data FlowMethod s a
  = Fork (Flow s) (Control s -> a)
  | DoAff (forall eff. AppFlow eff s) (s -> a)
  | Await (Control s) (s -> a)
  | Delay Milliseconds a
  | OneOf (Array (Flow s)) (s -> a)
  | UIFlow (forall eff. UIRunner -> AppFlow eff s) (s -> a)
  | PermissionFlow (forall eff. PermissionRunner -> AppFlow eff s) (s -> a)
  | APIFlow (forall eff. APIRunner -> AppFlow eff s) (s -> a)
  | Set Key String a
  | Get Key (Maybe String -> a)

instance functorFlowMethodF :: Functor (FlowMethod s) where
  map f (Fork g h) = Fork g (h >>> f)
  map f (DoAff g h) = DoAff g (h >>> f)
  map f (Await g h) = Await g (h >>> f)
  map f (Delay g h) = Delay g (f h)
  map f (OneOf g h) = OneOf g (h >>> f)
  map f (UIFlow g h) = UIFlow g (h >>> f)
  map f (PermissionFlow g h) = PermissionFlow g (h >>> f)
  map f (APIFlow g h) = APIFlow g (h >>> f)
  map f (Set k s h) = Set k s (f h)
  map f (Get k h)   = Get k (h >>> f)

newtype FlowWrapper a = FlowWrapper (Existing FlowMethod a)

instance functorFlowWrapper :: Functor FlowWrapper where
  map f (FlowWrapper g) = FlowWrapper $ mkExisting $ map f $ unExisting g

-- | Free monadic language for making flows.
newtype Flow a = Flow (Free FlowWrapper a)

unFlow :: forall a. Flow a -> Free FlowWrapper a
unFlow (Flow fl) = fl

instance functorFlow :: Functor Flow where
  map f (Flow fl) = Flow (f <$> fl)

instance applyFlow :: Apply Flow where
  apply (Flow f) (Flow fl) = Flow (apply f fl)

instance applicativeFlow :: Applicative Flow where
  pure = Flow <<< pure

instance bindFlow :: Bind Flow where
  bind (Flow fl) f = Flow $ bind fl (unFlow <<< f)

instance monadFlow :: Monad Flow

instance monadRecFlow :: MonadRec Flow where
  tailRecM k a = k a >>= case _ of
    Loop b -> tailRecM k b
    Done r -> pure r

-- | FlowWrapper for existential type.
wrap :: forall a s. FlowMethod s a -> Flow a
wrap = Flow <<< liftF <<< FlowWrapper <<< mkExisting

-- | Forks a flow and returns a control structure for getting results back (like Future).
fork :: forall s. Flow s -> Flow (Control s)
fork flow = wrap $ Fork flow id

-- | Forks a flow and returns a void control structure.
launch :: Flow Unit -> Flow (Control Unit)
launch = fork

-- | Runs any Aff as part of the flow
doAff :: forall s. (forall eff. AppFlow eff s) -> Flow s
doAff aff = wrap $ DoAff aff id

-- | Awaits result from a forked flow.
await :: forall s. Control s -> Flow s
await control = wrap $ Await control id

-- | Awaits a forked flow to be completed.
await' :: forall s. Control s -> Flow Unit
await' = void <<< await

-- | Delays computation for a given number of milliseconds.
delay :: Milliseconds -> Flow Unit
delay duration = wrap $ Delay duration unit

-- | Executes a set of actions and returns when the first one is done
oneOf :: forall s. Array (Flow s) -> Flow s
oneOf flows = wrap $ OneOf flows id

uiFlow :: forall s. (forall eff. UIRunner -> AppFlow eff s) -> Flow s
uiFlow aff = wrap $ UIFlow aff id

permissionFlow :: forall s. (forall eff. PermissionRunner -> AppFlow eff s) -> Flow s
permissionFlow aff = wrap $ PermissionFlow aff id

apiFlow :: forall s. (forall eff. APIRunner -> AppFlow eff s) -> Flow s
apiFlow aff = wrap $ APIFlow aff id

set :: Key -> String -> Flow Unit
set k v = wrap $ Set k v unit

get :: Key -> Flow (Maybe String)
get k = wrap $ Get k id