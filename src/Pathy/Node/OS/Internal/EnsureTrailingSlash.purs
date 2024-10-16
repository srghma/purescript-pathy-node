module Pathy.Node.OS.Internal.EnsureTrailingSlash where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String

ensureTrailingSlash :: String -> String
ensureTrailingSlash s =
  case String.charAt (String.length s - 1) s of
    Just '/' -> s
    _ -> s <> "/"
