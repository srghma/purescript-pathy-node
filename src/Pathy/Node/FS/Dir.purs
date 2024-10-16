module Pathy.Node.FS.Dir where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Dir as FS
import Pathy as P
import Pathy.Node.Internal.ParsePathWithPrettyErrorAuto (parsePathOrThrow)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.FS.Dir"

-- since our `printer sandboxedPath` can return only abs path - no need to worry for Rel paths

newtype Dir = Dir FS.Dir

derive newtype instance Show Dir

path :: Dir -> P.Path P.Abs P.Dir
path (Dir dir) = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath: FS.path dir, moduleName, functionName: "path" }
