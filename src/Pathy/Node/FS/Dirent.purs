module Pathy.Node.FS.Dirent (module Export, Dirent(..), parentPath, getType, name) where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Dirent (DirentType(..)) as Export
import Node.FS.Dirent as FS
import Pathy as P
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.FS.Dirent"

newtype Dirent = Dirent (FS.Dirent FS.DirentNameString)

derive newtype instance Show Dirent

-- since our `printer sandboxedPath` can return only abs path - no need to worry for Rel paths
parentPath :: Dirent -> P.Path P.Abs P.Dir
parentPath (Dirent dir) = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath: FS.parentPath dir, moduleName, functionName: "parentPath" }

getType :: Dirent -> FS.DirentType
getType (Dirent dirent) = FS.getType dirent

name :: Dirent -> String
name (Dirent dirent) = FS.name dirent
