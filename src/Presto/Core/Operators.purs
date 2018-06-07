module Presto.Core.Operators where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Presto.Core.Types.Language.Flow (Flow, Control, launch, fork)
import Presto.Core.Types.Language.Storage (class Serializable, Key)
import Presto.Core.Types.Language.Store (StoreF, load, save)
import Presto.Core.Types.Language.Types (run)

orRun :: forall m a. Monad m => m (Maybe a) -> m a -> m a
orRun value flow = do
  maybeValue <- value
  case maybeValue of
    Just v -> pure v
    Nothing -> flow

orElse :: forall m a. Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orElse f1 f2 = do
  result1 <- f1
  case result1 of
    Just val -> pure (Just val)
    Nothing -> f2

untilSuccessful :: forall m a. Monad m => m (Maybe a) -> m a
untilSuccessful flow = do
  result <- flow
  case result of
    Nothing -> untilSuccessful flow
    Just a -> pure a

until :: forall m a. Monad m => (a -> Boolean) -> m a -> m a
until pred flow = do
  result <- flow
  case pred result of
    false -> until pred flow
    true -> pure result

infixl 5 orElse as <|>

onFirstRun :: forall s. Serializable s => Key -> Flow s -> Flow s
onFirstRun key flow = do
  let loadVal :: Free StoreF (Maybe s)
      loadVal = load key
      saveVal :: s -> Free StoreF Unit
      saveVal v = save key v
  mbRes <- run loadVal
  case mbRes of
    Just value -> pure value
    Nothing -> do
      value <- flow
      run $ saveVal value
      pure value

inParallel :: Array (Flow Unit) -> Flow (Array (Control Unit))
inParallel = traverse fork

inParallel' :: Array (Flow Unit) -> Flow Unit
inParallel' = traverse_ launch
