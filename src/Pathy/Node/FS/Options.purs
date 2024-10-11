module Pathy.Node.FS.Options where

import Node.FS.Options (CpForce(..))
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Constants (CopyMode, copyFile_NO_FLAGS)
import Node.FS.Options as NodeFs
import Node.Path (FilePath)
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Pathy.Path (AbsAnyPathVariant)
import Type.Prelude (Proxy(..))

type CpFilterFunction a = a -> a -> Boolean

type CpDirOptions =
  { dereference :: Boolean
  , filter :: Maybe (CpFilterFunction AbsAnyPathVariant)
  , force :: CpForce
  , mode :: CopyMode
  , preserveTimestamps :: Boolean
  , verbatimSymlinks :: Boolean
  }

cpDirOptionsDefault :: CpDirOptions
cpDirOptionsDefault =
  { dereference: false
  , filter: Nothing
  , force: CpForce_TrueWithoutErrorOnExit
  , mode: copyFile_NO_FLAGS
  , preserveTimestamps: false
  , verbatimSymlinks: false
  }

moduleName :: String
moduleName = "Pathy.Node.FS.Options"

cpFilterFunction_toInternal__unsafeTo__AbsAnyPathVariant :: FilePath -> AbsAnyPathVariant
cpFilterFunction_toInternal__unsafeTo__AbsAnyPathVariant filePath = unsafePerformEffect $ parsePathOrThrow (Proxy :: _ "AbsAnyPathVariant") { filePath, moduleName, functionName: "cpDir" }

cpFilterFunction_toInternal :: CpFilterFunction AbsAnyPathVariant -> CpFilterFunction FilePath
cpFilterFunction_toInternal f from to = f (cpFilterFunction_toInternal__unsafeTo__AbsAnyPathVariant from) (cpFilterFunction_toInternal__unsafeTo__AbsAnyPathVariant to)

cpDirOptionsToCpOptionsInternal :: CpDirOptions -> NodeFs.CpDirOptions
cpDirOptionsToCpOptionsInternal
  { dereference
  , filter
  , force
  , mode
  , preserveTimestamps
  , verbatimSymlinks
  } =
  { dereference
  , force
  , mode
  , preserveTimestamps
  , verbatimSymlinks
  , filter: map cpFilterFunction_toInternal filter
  }
