module Pathy.Node.FS.Dir where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Dir as FS
import Pathy (class IsRelOrAbs)
import Pathy as P
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.FS.Dir"

newtype Dir :: forall k. k -> Type
newtype Dir relOrAbs = Dir FS.Dir

derive newtype instance Show (Dir relOrAbs)

-- class IsRelOrAbs relOrAbs <= HasPath dirRelOrAbs relOrAbs | dirRelOrAbs -> relOrAbs where
class HasPath dirRelOrAbs relOrAbs | dirRelOrAbs -> relOrAbs where
  path :: dirRelOrAbs -> P.Path relOrAbs P.Dir

instance HasPath (Dir P.Rel) P.Rel where
  path (Dir dir) = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Rel Dir") { filePath: FS.path dir, moduleName, functionName: "path" }
else instance HasPath (Dir P.Abs) P.Abs where
  path (Dir dir) = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath: FS.path dir, moduleName, functionName: "path" }
