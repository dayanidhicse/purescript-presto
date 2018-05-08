module Presto.Core.Language.Runtime.UI where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Bifunctor (rmap)
import Data.Exists (Exists, mkExists)
import Data.Tuple (Tuple)
import Presto.Core.Types.App (UIFlow)
import Presto.Core.Types.Language.Interaction (Interaction)
import Presto.Core.Types.Language.Pairing (class Pairing, pair)
import Presto.Core.Types.Language.UI (ErrorHandler, Gui, GuiF(..), GuiMethodF(..), UIResult)
import Unsafe.Coerce (unsafeCoerce)

newtype CoGuiMethodF s a = CoGuiMethodF {
    runUI :: (Interaction (UIResult s)) -> Tuple (UIResult s) a
  , forkUI :: (Interaction (UIResult s)) -> a
  , initUIWithScreen :: (forall eff. UIFlow eff s) -> Tuple s a
  , initUI :: (forall eff. UIFlow eff s) -> Tuple s a
  , runScreen :: (forall eff. UIFlow eff s) -> Tuple s a
  , forkScreen :: (forall eff. UIFlow eff s) -> a
  , handleError :: Gui (ErrorHandler s) -> Tuple s a
}

newtype CoGuiF a = CoGuiF (Exists (CoGuiMethodF a))

instance functorCoGuiMethodF :: Functor CoGuiF where
    map f (CoGuiF eo) = CoGuiF $ mkExists $ g $ unsafeCoerce eo
        where
            g (CoGuiMethodF co) = CoGuiMethodF {
                runUI: co.runUI >>> rmap f
              , forkUI: co.forkUI >>> f
              , initUIWithScreen: co.initUIWithScreen >>> rmap f
              , initUI: co.initUI >>> rmap f
              , runScreen: co.runScreen >>> rmap f
              , forkScreen: co.forkScreen >>> f
              , handleError: co.handleError >>> rmap f
            }

instance pairingCoGui :: Pairing CoGuiF GuiF where
    pair p (CoGuiF eo) (GuiF ao) = doPair (unsafeCoerce eo) (unsafeCoerce ao)
        where
            doPair (CoGuiMethodF co) (RunUI req next) = pair p (co.runUI req) next
            doPair (CoGuiMethodF co) (ForkUI req next) = p (co.forkUI req) next
            doPair (CoGuiMethodF co) (InitUIWithScreen req next) = pair p (co.initUIWithScreen req) next
            doPair (CoGuiMethodF co) (InitUI req next) = pair p (co.initUI req) next
            doPair (CoGuiMethodF co) (RunScreen req next) = pair p (co.runScreen req) next
            doPair (CoGuiMethodF co) (ForkScreen req next) = p (co.forkScreen req) next
            doPair (CoGuiMethodF co) (HandleError req next) = pair p (co.handleError req) next

type CoGui = Cofree CoGuiF
