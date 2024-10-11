module Pathy.Node.FS.Dirent where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Dirent as FS
import Pathy as P
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.FS.Dirent"

newtype Dirent = Dirent (FS.Dirent FS.DirentNameTypeString)

derive newtype instance Show Dirent

-- since our `printer sandboxedPath` can return only abs path - no need to worry for Rel paths
parentPath :: Dirent -> P.Path P.Abs P.Dir
parentPath (Dirent dir) = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath: FS.parentPath dir, moduleName, functionName: "parentPath" }

isBlockDevice :: Dirent -> Boolean
isBlockDevice (Dirent dirent) = FS.isBlockDevice dirent

isCharacterDevice :: Dirent -> Boolean
isCharacterDevice (Dirent dirent) = FS.isCharacterDevice dirent

isDirectory :: Dirent -> Boolean
isDirectory (Dirent dirent) = FS.isDirectory dirent

isFIFO :: Dirent -> Boolean
isFIFO (Dirent dirent) = FS.isFIFO dirent

isFile :: Dirent -> Boolean
isFile (Dirent dirent) = FS.isFile dirent

isSocket :: Dirent -> Boolean
isSocket (Dirent dirent) = FS.isSocket dirent

isSymbolicLink :: Dirent -> Boolean
isSymbolicLink (Dirent dirent) = FS.isSymbolicLink dirent

name :: Dirent -> String
name (Dirent dirent) = FS.name dirent
