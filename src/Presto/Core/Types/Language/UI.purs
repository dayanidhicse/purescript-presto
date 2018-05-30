module Presto.Core.Types.Language.UI where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Free (Free, liftF)
import DOM (DOM)
import Data.Either (Either, either)
import FRP (FRP)
import Presto.Core.Types.Language.Interaction (class Interact, Interaction, interact, interactConv)
import Presto.Core.Utils.Existing (Existing, mkExisting, unExisting)
import PrestoDOM.Core (runScreen, initUI, initUIWithScreen) as PrestoDOM
import PrestoDOM.Types.Core (Screen)

type UIFlow eff = Aff (frp :: FRP, dom :: DOM, ref :: REF | eff)
type UIResult s = Either Error s

data ErrorHandler s
  = ThrowError String
  | ReturnResult s

data GuiMethodF s a
    = RunUI (Interaction (UIResult s)) (UIResult s -> a)
    | ForkUI (Interaction (UIResult s)) a
    | InitUIWithScreen (forall eff. UIFlow eff s) (s -> a)
    | InitUI (forall eff. UIFlow eff s) (s -> a)
    | RunScreen (forall eff. UIFlow eff s) (s -> a)
    | ForkScreen (forall eff. UIFlow eff s) a
    | HandleError (Gui (ErrorHandler s)) (s -> a)

instance functorGuiMethodF :: Functor (GuiMethodF s) where
  map f (RunUI g h) = RunUI g (h >>> f)
  map f (ForkUI g h) = ForkUI g (f h)
  map f (InitUIWithScreen g h) = InitUIWithScreen g (h >>> f)
  map f (InitUI g h) = InitUI g (h >>> f)
  map f (RunScreen g h) = RunScreen g (h >>> f)
  map f (ForkScreen g h) = ForkScreen g (f h)
  map f (HandleError g h) = HandleError g (h >>> f)

newtype GuiF a = GuiF (Existing GuiMethodF a)

instance functorGui :: Functor GuiF where
    map f (GuiF g) = GuiF $ mkExisting $ map f $ unExisting g

type Gui = Free GuiF

wrap :: forall a s. GuiMethodF s a -> Gui a
wrap = liftF <<< GuiF <<< mkExisting

-- | Converts error to string and throws at runtime or returns result.
withError :: forall err s. (err -> String) -> Gui (Either err s) -> Gui s
withError toMsg flow = wrap $ HandleError flow' id
  where
    flow' = flow >>= either (pure <<< ThrowError <<< toMsg) (pure <<< ReturnResult)

-- | Runs UI and returns result of user's interaction with a screen.
runUI' :: forall a b. Interact Error a b => a -> Gui (UIResult b)
runUI' a = wrap $ RunUI (interact a) id

-- | Runs UI and returns result of user's interaction with a screen.
-- | Handles error in runtime.
runUI :: forall a b. Interact Error a b => a -> Gui b
runUI = withError show <<< runUI'

-- | Runs UI async, doesn't return anything useful.
-- | Handles error in runtime.
forkUI :: forall a b. Interact Error a b => a -> Gui Unit
forkUI a = wrap $ ForkUI (interact a) unit

-- | Runs UI and doesn't return anything useful (equivalent to `void <<< runUI`).
-- | Handles error in runtime.
showUI :: forall a b. Interact Error a b => a -> Gui Unit
showUI = void <<< runUI

-- | Runs UI with a custom converter
-- | Handles error in runtime.
evalUI :: forall a b s. Interact Error a b => a -> (b -> Either Error s) -> Gui s
evalUI a from = withError show $ wrap $ RunUI (interactConv a from) id

-- | Initialize all states and machines required by PrestoDOM. Returns control back immediately.
initUI :: Gui Unit
initUI = wrap $ InitUI (makeAff (\cb -> PrestoDOM.initUI cb)) id

-- | Initialize all states and machines required by PrestoDOM. Takes a PrestoDOM Screen and returns control back immediately.
initUIWithScreen :: forall action state. (forall eff. Screen action state eff Unit) -> Gui Unit
initUIWithScreen screen = wrap $ InitUIWithScreen (makeAff (\cb -> PrestoDOM.initUIWithScreen screen cb)) id

-- | Runs PrestoDOM Screen and returns the result. In this case, the whole screen is rerendered.
runScreen :: forall action state s. (forall eff. Screen action state eff s) -> Gui s
runScreen screen = wrap $ RunScreen (makeAff (\cb -> PrestoDOM.runScreen screen cb)) id

-- | Forks PrestoDOM Screen and returns control back immediately.
forkScreen :: forall action state s. (forall eff. Screen action state eff s) -> Gui Unit
forkScreen screen = wrap $ ForkScreen (makeAff (\cb -> PrestoDOM.runScreen screen cb)) unit

-- | Throws error.
throwErr :: forall a. String -> Gui a
throwErr msg = wrap $ HandleError flow' id
  where
    flow' = pure $ ThrowError msg
