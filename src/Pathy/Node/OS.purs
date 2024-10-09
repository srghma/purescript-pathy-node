module Pathy.Node.OS where

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
import Node.OS as OS
import Node.Path (FilePath)
import Pathy (Abs, AbsAnyPathVariant, Dir, Parser, Printer, Rel, posixParser, posixPrinter, windowsPrinter)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser)
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Pathy.Parser as P
import Pathy.Phantom (RelOrAbs)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.OS"

tmpdir :: Effect AbsDir
tmpdir = OS.tmpdir >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "tmpdir" }

homedir :: Effect AbsDir
homedir = OS.homedir >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "homedir" }

devNull :: AbsFile
devNull = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Abs File") { filePath: OS.devNull, moduleName, functionName: "devNull" }
