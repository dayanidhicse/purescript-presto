module Presto.Core.Types.Language.Flow where

import Prelude

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Free (Free, liftF)
import Data.Time.Duration (Milliseconds)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Utils.Existing (Existing, mkExisting, unExisting)

newtype Control s = Control (AVar s)

-- | Algebra of the Flow free language.
data FlowMethod s a
  = Fork (Flow s) (Control s -> a)
  | DoAff (forall eff. AppFlow eff s) (s -> a)
  | Await (Control s) (s -> a)
  | Delay Milliseconds a
  | OneOf (Array (Flow s)) (s -> a)

instance functorFlowMethodF :: Functor (FlowMethod s) where
  map f (Fork g h) = Fork g (h >>> f)
  map f (DoAff g h) = DoAff g (h >>> f)
  map f (Await g h) = Await g (h >>> f)
  map f (Delay g h) = Delay g (f h)
  map f (OneOf g h) = OneOf g (h >>> f)

newtype FlowWrapper a = FlowWrapper (Existing FlowMethod a)

instance functorFlowWrapper :: Functor FlowWrapper where
  map f (FlowWrapper g) = FlowWrapper $ mkExisting $ map f $ unExisting g

-- | Free monadic language for making flows.
type Flow a = Free FlowWrapper a

-- | FlowWrapper for existential type.
wrap :: forall a s. FlowMethod s a -> Flow a
wrap = liftF <<< FlowWrapper <<< mkExisting

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
oneOf :: forall m s. Array (Flow s) -> Flow s
oneOf flows = wrap $ OneOf flows id
