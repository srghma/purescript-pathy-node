module Pathy.Node.FS.Aff
  ( access
  , access'
  , appendFile
  , appendFile'
  , appendTextFile
  , chmod
  , chown
  , copyFile
  , copyFile'
  , cp
  , cp'
  , fdOpen
  , glob
  , glob'
  , globDirent
  , globDirent'
  , lchmod
  , lchown
  , link
  , lstat
  , lutimes
  , mkdir
  , mkdir'
  , mkdtemp
  , mkdtemp'
  , opendir
  , opendir'
  , readFile
  , readFile'
  , readTextFile
  , readTextFile'
  , readdir
  , readdir'
  , readdirBuffer
  , readdirBuffer'
  , readdirDirent
  , readdirDirent'
  , readdirDirentBuffer
  , readdirDirentBuffer'
  , readlink
  , readlinkBuffer
  , realpath
  , realpath'
  , rename
  , rm
  , rm'_dir
  , rm'_file
  , rmdir
  , rmdir'
  , stat
  , statfs
  , symlink
  , truncate
  , unlink
  , utimes
  , writeFile
  , writeFile'
  , writeTextFile
  , writeTextFile'
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.FS.Aff as F
import Node.FS.Constants (AccessMode, CopyMode, FileFlags)
import Node.FS.Dirent as FS
import Node.FS.Options
import Node.FS.Perms (Perms)
import Node.FS.Stats (Stats)
import Node.FS.Types (FileDescriptor, FileMode, SymlinkType)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, File, printPath)
import Pathy.Node.FS.Dir (Dir(..)) as PathyFs
import Pathy.Node.FS.Dirent (Dirent(..)) as PathyFs
import Pathy.Node.Internal.Utils (parsePathOrThrow, class AnyDirToVariant)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentPrinter)
import Pathy.Path (AbsAnyPathVariant, AbsDir, AnyAnyPathVariant)
import Pathy.Sandboxed (SandboxedPath)
import Type.Prelude (Proxy(..))

moduleName :: String
moduleName = "Pathy.Node.FS"

access :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Aff (Maybe Error)
access path = F.access (printPath currentPrinter path)

access' :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> AccessMode -> Aff (Maybe Error)
access' path = F.access' (printPath currentPrinter path)

copyFile :: forall a b. IsRelOrAbs a => IsRelOrAbs b => SandboxedPath File -> SandboxedPath File -> Aff Unit
copyFile fromPath toPath = F.copyFile (printPath currentPrinter fromPath) (printPath currentPrinter toPath)

copyFile' :: forall a b. IsRelOrAbs a => IsRelOrAbs b => SandboxedPath File -> SandboxedPath File -> CopyMode -> Aff Unit
copyFile' src dest = F.copyFile' (printPath currentPrinter src) (printPath currentPrinter dest)

-- Due to platform inconsistencies, avoid trailing X characters in prefix
-- e.g. 'foo-'
-- newtype MkdtempPrefix = MkdtempPrefix String

mkdtemp :: SandboxedPath File -> Aff AbsDir
mkdtemp file = F.mkdtemp (printPath currentPrinter file) >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "mkdtemp" }

mkdtemp' :: SandboxedPath File -> Encoding -> Aff AbsDir
mkdtemp' file encoding = F.mkdtemp' (printPath currentPrinter file) encoding >>= \filePath -> parsePathOrThrow (Proxy :: _ "Path Abs Dir") { filePath, moduleName, functionName: "mkdtemp'" }

rename :: forall a c. IsRelOrAbs a => IsRelOrAbs c => SandboxedPath File -> SandboxedPath File -> Aff Unit
rename oldPath newPath = F.rename (printPath currentPrinter oldPath) (printPath currentPrinter newPath)

truncate :: SandboxedPath File -> Int -> Aff Unit
truncate file len = F.truncate (printPath currentPrinter file) len

chown :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Int -> Int -> Aff Unit
chown path uid gid = F.chown (printPath currentPrinter path) uid gid

chmod :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Perms -> Aff Unit
chmod path perms = F.chmod (printPath currentPrinter path) perms

stat :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Aff Stats
stat path = F.stat (printPath currentPrinter path)

lstat :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Aff Stats
lstat path = F.lstat (printPath currentPrinter path)

-- hardlink for a file, no dirs
link :: forall a b. IsRelOrAbs a => IsRelOrAbs b => SandboxedPath File -> SandboxedPath File -> Aff Unit
link existingPath newPath = F.link (printPath currentPrinter existingPath) (printPath currentPrinter newPath)

-- creates
symlink :: forall a b c d. IsRelOrAbs a => IsRelOrAbs c => IsDirOrFile b => IsDirOrFile d => SandboxedPath b -> SandboxedPath d -> SymlinkType -> Aff Unit
symlink target path type_ = F.symlink (printPath currentPrinter target) (printPath currentPrinter path) type_

-- reads
readlink :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Aff AnyAnyPathVariant
readlink path = F.readlink (printPath currentPrinter path) >>= \filePath -> parsePathOrThrow
  (Proxy :: _ "AnyAnyPathVariant")
  { filePath
  , moduleName
  , functionName: "readlink"
  }

readlinkBuffer :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Aff Buffer
readlinkBuffer path = F.readlinkBuffer (printPath currentPrinter path)

realpath :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Aff AbsAnyPathVariant -- TODO: read stats, if dir - add / to the end to parse ad dir
realpath path = F.realpath (printPath currentPrinter path) >>= \filePath -> parsePathOrThrow
  (Proxy :: _ "AbsAnyPathVariant")
  { filePath
  , moduleName
  , functionName: "realpath"
  }

realpath' :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> RealpathOptions -> Aff AbsAnyPathVariant
realpath' path options = F.realpath' (printPath currentPrinter path) options >>= \filePath -> parsePathOrThrow
  (Proxy :: _ "AbsAnyPathVariant")
  { filePath
  , moduleName
  , functionName: "realpath'"
  }

unlink :: SandboxedPath File -> Aff Unit
unlink path = F.unlink (printPath currentPrinter path)

rmdir :: SandboxedPath Dir -> Aff Unit
rmdir path = F.rmdir (printPath currentPrinter path)

rmdir' :: SandboxedPath Dir -> RmdirOptions -> Aff Unit
rmdir' path = F.rmdir' (printPath currentPrinter path)

rm :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Aff Unit
rm path = F.rm (printPath currentPrinter path)

type RmOptions_Dir = RmOptions

rmOptionsDefault_Dir :: RmOptions_Dir
rmOptionsDefault_Dir = rmOptionsDefault

type RmOptions_File = { force :: Boolean, maxRetries :: Int, retryDelay :: Int }

rmOptionsDefault_File :: RmOptions_File
rmOptionsDefault_File = { force: false, maxRetries: 100, retryDelay: 1000 }

rm'_dir :: SandboxedPath Dir -> RmOptions_Dir -> Aff Unit
rm'_dir path options = F.rm' (printPath currentPrinter path) options

rm'_file :: SandboxedPath File -> RmOptions_File -> Aff Unit
rm'_file path { force, maxRetries, retryDelay } = F.rm' (printPath currentPrinter path) { recursive: false, force, maxRetries, retryDelay }

mkdir :: SandboxedPath Dir -> Aff Unit
mkdir path = F.mkdir (printPath currentPrinter path)

mkdir' :: SandboxedPath Dir -> MkdirOptions -> Aff Unit
mkdir' path options = F.mkdir' (printPath currentPrinter path) options

readdir
  :: forall relOrAbs relOrAbs_AnyPathVariant anyPathVariant_symbol
   . IsRelOrAbs relOrAbs
  => AnyDirToVariant anyPathVariant_symbol relOrAbs relOrAbs_AnyPathVariant
  => SandboxedPath Dir
  -> Aff (Array AbsAnyPathVariant)
readdir path = F.readdir (printPath currentPrinter path) >>= traverse \dirpath ->
  parsePathOrThrow
    (Proxy :: _ "AbsAnyPathVariant")
    { filePath: dirpath
    , moduleName: "Pathy.Node.FS.Dir"
    , functionName: "readdir"
    }

readdir'
  :: forall relOrAbs relOrAbs_AnyPathVariant anyPathVariant_symbol
   . IsRelOrAbs relOrAbs
  => AnyDirToVariant anyPathVariant_symbol relOrAbs relOrAbs_AnyPathVariant
  => SandboxedPath Dir
  -> ReaddirFilePathOptions
  -> Aff (Array relOrAbs_AnyPathVariant)
readdir' path options = F.readdir' (printPath currentPrinter path) options >>= traverse \dirpath ->
  parsePathOrThrow
    (Proxy :: _ anyPathVariant_symbol)
    { filePath: dirpath
    , moduleName: "Pathy.Node.FS.Dir"
    , functionName: "readdir'"
    }

readdirBuffer :: SandboxedPath Dir -> Aff (Array Buffer)
readdirBuffer path = F.readdirBuffer (printPath currentPrinter path)

-- | Reads the contents of a directory with options and returns Aff (Array Buffer).
readdirBuffer' :: SandboxedPath Dir -> ReaddirBufferOptions -> Aff (Array Buffer)
readdirBuffer' path = F.readdirBuffer' (printPath currentPrinter path)

-- | Reads the contents of a directory and returns an Aff (Array (Dirent DirentNameTypeString)).
readdirDirent :: SandboxedPath Dir -> Aff (Array (PathyFs.Dirent Abs))
readdirDirent path = map (map PathyFs.Dirent) $ F.readdirDirent (printPath currentPrinter path)

-- | Reads the contents of a directory with options and returns Aff (Array (Dirent DirentNameTypeString)).
readdirDirent' :: SandboxedPath Dir -> ReaddirDirentOptions -> Aff (Array (PathyFs.Dirent Abs))
readdirDirent' path options = map (map PathyFs.Dirent) $ F.readdirDirent' (printPath currentPrinter path) options

-- | Reads the contents of a directory.
readdirDirentBuffer :: SandboxedPath Dir -> Aff (Array (FS.Dirent FS.DirentNameTypeBuffer))
readdirDirentBuffer path = F.readdirDirentBuffer (printPath currentPrinter path)

-- | Reads the contents of a directory.
readdirDirentBuffer' :: SandboxedPath Dir -> ReaddirDirentBufferOptions -> Aff (Array (FS.Dirent FS.DirentNameTypeBuffer))
readdirDirentBuffer' path options = F.readdirDirentBuffer' (printPath currentPrinter path) options

utimes :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> DateTime -> DateTime -> Aff Unit
utimes path atime mtime = F.utimes (printPath currentPrinter path) atime mtime

readFile :: SandboxedPath File -> Aff Buffer
readFile path = F.readFile (printPath currentPrinter path)

readFile' :: SandboxedPath File -> ReadFileBufferOptions -> Aff Buffer
readFile' path = F.readFile' (printPath currentPrinter path)

readTextFile :: Encoding -> SandboxedPath File -> Aff String
readTextFile encoding path = F.readTextFile encoding (printPath currentPrinter path)

readTextFile' :: SandboxedPath File -> ReadFileStringOptions -> Aff String
readTextFile' path = F.readTextFile' (printPath currentPrinter path)

writeFile :: SandboxedPath File -> Buffer -> Aff Unit
writeFile path = F.writeFile (printPath currentPrinter path)

writeFile' :: SandboxedPath File -> Buffer -> WriteFileBufferOptions -> Aff Unit
writeFile' path = F.writeFile' (printPath currentPrinter path)

writeTextFile :: Encoding -> SandboxedPath File -> String -> Aff Unit
writeTextFile encoding path text = F.writeTextFile encoding (printPath currentPrinter path) text

writeTextFile' :: SandboxedPath File -> String -> WriteFileStringOptions -> Aff Unit
writeTextFile' path = F.writeTextFile' (printPath currentPrinter path)

appendFile :: SandboxedPath File -> Buffer -> Aff Unit
appendFile path buffer = F.appendFile (printPath currentPrinter path) buffer

appendFile' :: SandboxedPath File -> Buffer -> AppendFileBufferOptions -> Aff Unit
appendFile' path buffer = F.appendFile' (printPath currentPrinter path) buffer

appendTextFile :: Encoding -> SandboxedPath File -> String -> Aff Unit
appendTextFile encoding path text = F.appendTextFile encoding (printPath currentPrinter path) text

fdOpen :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> FileFlags -> Maybe FileMode -> Aff FileDescriptor
fdOpen path = F.fdOpen (printPath currentPrinter path)

cp :: forall relOrAbs1 relOrAbs2 dirOrFile. IsRelOrAbs relOrAbs1 => IsRelOrAbs relOrAbs2 => IsDirOrFile dirOrFile => SandboxedPath dirOrFile -> SandboxedPath dirOrFile -> Aff Unit
cp fromPath toPath = F.cp (printPath currentPrinter fromPath) (printPath currentPrinter toPath)

cp' :: forall relOrAbs1 relOrAbs2 dirOrFile. IsRelOrAbs relOrAbs1 => IsRelOrAbs relOrAbs2 => IsDirOrFile dirOrFile => SandboxedPath dirOrFile -> SandboxedPath dirOrFile -> CpOptions -> Aff Unit
cp' fromPath toPath = F.cp' (printPath currentPrinter fromPath) (printPath currentPrinter toPath)

glob
  :: forall relOrAbs relOrAbs_AnyPathVariant anyPathVariant_symbol
   . IsRelOrAbs relOrAbs
  => AnyDirToVariant anyPathVariant_symbol relOrAbs relOrAbs_AnyPathVariant
  => Array (SandboxedPath File)
  -> Aff (Array relOrAbs_AnyPathVariant)
glob path = F.glob (map (printPath currentPrinter) path) >>= traverse (\filePath -> parsePathOrThrow (Proxy :: _ anyPathVariant_symbol) { filePath, moduleName: "Pathy.Node.FS.Dir", functionName: "path" })

glob'
  :: forall relOrAbs relOrAbs_AnyPathVariant anyPathVariant_symbol
   . IsRelOrAbs relOrAbs
  => AnyDirToVariant anyPathVariant_symbol relOrAbs relOrAbs_AnyPathVariant
  => Array (SandboxedPath File)
  -> GlobFilePathOptions
  -> Aff (Array relOrAbs_AnyPathVariant)
glob' paths options = do
  let filePaths = map (printPath currentPrinter) paths
  result <- F.glob' filePaths options
  traverse (\filePath -> parsePathOrThrow (Proxy :: _ anyPathVariant_symbol) { filePath, moduleName, functionName: "glob'" }) result

globDirent
  :: forall relOrAbs
   . IsRelOrAbs relOrAbs
  => Array (SandboxedPath File)
  -> Aff (Array (PathyFs.Dirent Abs))
globDirent paths = do
  let filePaths = map (printPath currentPrinter) paths
  arrayDirent <- F.globDirent filePaths
  pure $ map PathyFs.Dirent arrayDirent

globDirent'
  :: forall relOrAbs
   . IsRelOrAbs relOrAbs
  => Array (SandboxedPath File)
  -> GlobDirentOptions
  -> Aff (Array (PathyFs.Dirent Abs))
globDirent' paths options = do
  let filePaths = map (printPath currentPrinter) paths
  arrayDirent <- F.globDirent' filePaths options
  pure $ map PathyFs.Dirent arrayDirent

lchmod :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Perms -> Aff Unit
lchmod path = F.lchmod (printPath currentPrinter path)

lchown :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> Int -> Int -> Aff Unit
lchown path = F.lchown (printPath currentPrinter path)

lutimes :: forall a b. IsRelOrAbs a => IsDirOrFile b => SandboxedPath b -> DateTime -> DateTime -> Aff Unit
lutimes path = F.lutimes (printPath currentPrinter path)

opendir :: SandboxedPath Dir -> Aff (PathyFs.Dir Abs)
opendir path = map PathyFs.Dir $ F.opendir (printPath currentPrinter path)

opendir' :: SandboxedPath Dir -> OpendirOptions -> Aff (PathyFs.Dir Abs)
opendir' path options = map PathyFs.Dir $ F.opendir' (printPath currentPrinter path) options

statfs :: forall b. IsDirOrFile b => SandboxedPath b -> Aff Stats
statfs path = F.statfs (printPath currentPrinter path)
