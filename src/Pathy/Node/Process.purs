module Pathy.Node.Process where

import Pathy.Path (AbsDir)
import Prelude

import Effect (Effect)
import Node.Process as Process
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.Process"

cwd :: Effect AbsDir
cwd = Process.cwd >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "cwd" }

execPath :: Effect AbsDir
execPath = Process.execPath >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "execPath" }
