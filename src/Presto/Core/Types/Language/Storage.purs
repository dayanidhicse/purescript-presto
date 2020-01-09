module Presto.Core.Types.Language.Storage
  ( class Serializable
  , Key
  , deserialize
  , serialize
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, encodeJSON)

type Key = String

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> Maybe a

instance genericSerializable :: (Encode a, Decode a) => Serializable a where
  serialize = encodeJSON
  deserialize = decodeJSON >>> runExcept >>> hush