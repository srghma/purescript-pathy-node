module Pathy.Node.FS.Options where

import Node.FS.Options
import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.FS.Aff as F
import Node.FS.Constants (AccessMode, CopyMode, FileFlags, copyFile_NO_FLAGS)
import Node.FS.Dirent as FS
import Node.FS.Options as NodeFs
import Node.FS.Perms (Perms)
import Node.FS.Stats (Stats)
import Node.FS.Types (FileDescriptor, FileMode, SymlinkType)
import Node.Path (FilePath)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, AbsFile, Dir, File, Path, parsePath, printPath)
import Pathy.Node.FS.Dir (Dir(..)) as PathyFS
import Pathy.Node.FS.Dirent (Dirent(..)) as PathyFS
import Pathy.Node.Internal.Utils (parsePathOrThrow)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser, currentPrinter)
import Pathy.Path (AbsAnyPathVariant, AbsDir, AnyAnyPathVariant)
import Pathy.Sandboxed (SandboxedPath)
import Record as Record
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
