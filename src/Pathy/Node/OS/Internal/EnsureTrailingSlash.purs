module Pathy.Node.OS.Internal.EnsureTrailingSlash where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Node.Path (FilePath)
import Pathy (AbsAnyPathVariant, AnyAnyPathVariant, Parser, Path, RelAnyPathVariant)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser)
import Pathy.Node.OS.Internal.ParsePathWithPrettyError as Internal
import Pathy.Parser (parseAbsAnyPathVariant, parseAbsDir, parseAbsFile, parseAnyAnyPathVariant, parseRelAnyPathVariant, parseRelDir, parseRelFile) as Pathy
import Pathy.Phantom (Abs, Dir, File, Rel, RelOrAbs)
import Type.Prelude (Proxy(..))

ensureTrailingSlash :: String -> String
ensureTrailingSlash s =
  case String.charAt (String.length s - 1) s of
    Just '/' -> s
    _ -> s <> "/"
