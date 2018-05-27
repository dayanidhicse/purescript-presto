module Presto.Core.Types.Language.Flow where

import Prelude

import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (Free, liftF)
import Data.Either (Either, either)
import Data.Exists (Exists)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Presto.Core.Types.API (class RestEndpoint, ErrorResponse, Headers, RegTokens)
import Presto.Core.Types.App (AppFlow, UIFlow)
import Presto.Core.Types.Language.Interaction (class Interact, Interaction, interact, interactConv)
import PrestoDOM.Core (runScreen, initUI, initUIWithScreen) as PrestoDOM
import PrestoDOM.Types.Core (Screen)
import Unsafe.Coerce (unsafeCoerce)

newtype Control s = Control (AVar s)

-- | Algebra of the Flow free language.
data FlowMethodF a s
  = Fork (Flow s) (Control s -> a)
  | DoAff (forall eff. AppFlow eff s) (s -> a)
  | Await (Control s) (s -> a)
  | Delay Milliseconds a
  | OneOf (Array (Flow s)) (s -> a)

type FlowMethod s a = FlowMethodF a s

newtype FlowWrapper a = FlowWrapper (Exists (FlowMethodF a))

-- | Free monadic language for making flows.
type Flow a = Free FlowWrapper a

-- | FlowWrapper for existential type.
wrap :: forall a s. FlowMethodF a s -> Flow a
wrap = liftF <<< FlowWrapper <<< unsafeCoerce

-- | Forks a flow and returns a control structure for getting results back (like Future).
fork :: forall s. Flow s -> Flow (Control s)
fork flow = wrap $ Fork flow id

-- | Forks a flow and returns a void control structure.
launch :: Flow Unit -> Flow (Control Unit)
launch flow = wrap $ Fork flow id

-- | Runs any Aff as part of the flow
doAff :: forall s. (forall eff. AppFlow eff s) -> Flow s
doAff aff = wrap $ DoAff aff id

-- | Awaits result from a forked flow.
await :: forall s. Control s -> Flow s
await control = wrap $ Await control id

-- | Awaits a forked flow to be completed.
await' :: forall s. Control s -> Flow Unit
await' control = void $ wrap $ Await control id

-- | Delays computation for a given number of milliseconds.
delay :: Milliseconds -> Flow Unit
delay duration = wrap $ Delay duration unit

-- | Executes a set of actions and returns when the first one is done
oneOf :: forall m s. Array (Flow s) -> Flow s
oneOf flows = wrap $ OneOf flows id
