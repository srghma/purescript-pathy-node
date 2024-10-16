module Pathy.Node.OS where

import Pathy.Path (AbsDir, AbsFile)
import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.OS as OS
import Pathy.Node.Internal.ParsePathWithPrettyErrorAuto (parsePathOrThrow)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.OS"

tmpdir :: Effect AbsDir
tmpdir = OS.tmpdir >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "tmpdir" }

homedir :: Effect AbsDir
homedir = OS.homedir >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "homedir" }

devNull :: AbsFile
devNull = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Abs File") { filePath: OS.devNull, moduleName, functionName: "devNull" }
