module Pathy.Node.Process where

import Pathy.Parser
import Pathy.Path
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Node.Process as Process
import Node.Path (FilePath)
import Pathy (Abs, AbsAnyPathVariant, Dir, Parser, Printer, Rel, posixParser, posixPrinter, windowsPrinter)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser)
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Pathy.Parser as P
import Pathy.Phantom (RelOrAbs)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.Process"

cwd :: Effect AbsDir
cwd = Process.cwd >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "cwd" }

execPath :: Effect AbsDir
execPath = Process.execPath >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "execPath" }
