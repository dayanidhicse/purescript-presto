module Presto.Core.Types.Language.UI where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Free (Free)
import DOM (DOM)
import Data.Either (Either, either)
import FRP (FRP)
import Presto.Core.Types.Language.Interaction (class Interact, Interaction, interact, interactConv)
import Presto.Core.Utils.Existing (Existing, mkExisting, unExisting)
import Presto.Core.Utils.Inject (class Inject, inject)
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
    | HandleError (ErrorHandler s) (s -> a)

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

wrap :: forall a s f. Inject GuiF f => GuiMethodF s a -> Free f a
wrap = inject <<< GuiF <<< mkExisting

-- | Converts error to string and throws at runtime or returns result.
withError :: forall err s f. Inject GuiF f => (err -> String) -> Free f (Either err s) -> Free f s
withError toMsg flow = do
  res <- flow
  wrap $ HandleError (either (ThrowError <<< toMsg) ReturnResult res) id

-- | Runs UI and returns result of user's interaction with a screen.
runUI' :: forall a b f. Inject GuiF f => Interact Error a b => a -> Free f (UIResult b)
runUI' a = wrap $ RunUI (interact a) id

-- | Runs UI and returns result of user's interaction with a screen.
-- | Handles error in runtime.
runUI :: forall a b f. Inject GuiF f => Interact Error a b => a -> Free f b
runUI = withError show <<< runUI'

-- | Runs UI async, doesn't return anything useful.
-- | Handles error in runtime.
forkUI :: forall a b f. Inject GuiF f => Interact Error a b => a -> Free f Unit
forkUI a = wrap $ ForkUI (interact a) unit

-- | Runs UI and doesn't return anything useful (equivalent to `void <<< runUI`).
-- | Handles error in runtime.
showUI :: forall a b f. Inject GuiF f => Interact Error a b => a -> Free f Unit
showUI = void <<< runUI

-- | Runs UI with a custom converter
-- | Handles error in runtime.
evalUI :: forall a b s f. Inject GuiF f => Interact Error a b => a -> (b -> Either Error s) -> Free f s
evalUI a from = withError show $ wrap $ RunUI (interactConv a from) id

-- | Initialize all states and machines required by PrestoDOM. Returns control back immediately.
initUI :: forall f. Inject GuiF f => Free f Unit
initUI = wrap $ InitUI (makeAff (\cb -> PrestoDOM.initUI cb)) id

-- | Initialize all states and machines required by PrestoDOM. Takes a PrestoDOM Screen and returns control back immediately.
initUIWithScreen :: forall action state f. Inject GuiF f => (forall eff. Screen action state eff Unit) -> Free f Unit
initUIWithScreen screen = wrap $ InitUIWithScreen (makeAff (\cb -> PrestoDOM.initUIWithScreen screen cb)) id

-- | Runs PrestoDOM Screen and returns the result. In this case, the whole screen is rerendered.
runScreen :: forall action state s f. Inject GuiF f => (forall eff. Screen action state eff s) -> Free f s
runScreen screen = wrap $ RunScreen (makeAff (\cb -> PrestoDOM.runScreen screen cb)) id

-- | Forks PrestoDOM Screen and returns control back immediately.
forkScreen :: forall action state s f. Inject GuiF f => (forall eff. Screen action state eff s) -> Free f Unit
forkScreen screen = wrap $ ForkScreen (makeAff (\cb -> PrestoDOM.runScreen screen cb)) unit

-- | Throws error.
throwErr :: forall a f. Inject GuiF f => String -> Free f a
throwErr msg = wrap $ HandleError (ThrowError msg) id
