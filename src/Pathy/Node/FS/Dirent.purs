module Pathy.Node.FS.Dirent where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Dirent as FS
import Pathy as P
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.FS.Dirent"

newtype Dirent :: forall k. k -> Type
newtype Dirent relOrAbs = Dirent (FS.Dirent FS.DirentNameTypeString)

derive newtype instance Show (Dirent relOrAbs)

class HasParentPath dirRelOrAbs relOrAbs | dirRelOrAbs -> relOrAbs where
  parentPath :: dirRelOrAbs -> P.Path relOrAbs P.Dir

instance HasParentPath (Dirent P.Rel) P.Rel where
  parentPath (Dirent dir) = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Rel Dir") { filePath: FS.parentPath dir, moduleName , functionName: "parentPath" }
else instance HasParentPath (Dirent P.Abs) P.Abs where
  parentPath (Dirent dir) = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath: FS.parentPath dir , moduleName , functionName: "parentPath" }
