module Test.PathyFS.OpendirSpec where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array ((!!))
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Node.Encoding (Encoding(..))
import Node.FS.Options (opendirOptionsDefault, rmOptionsDefault) as A
import Node.FS.Perms (permsAll)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, printPath)
import Pathy.Node.FS.Aff (mkdir, mkdir', opendir', rm'_dir, writeTextFile) as A
import Pathy.Node.FS.Aff.Dir (entries)
import Pathy.Node.FS.Dir (path) as PathyFS
import Pathy.Node.FS.Dirent (Dirent)
import Pathy.Node.FS.Dirent (Dirent, parentPath) as PathyFS
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentPrinter)
import Pathy.Node.Process (cwd) as PathyFS
import Pathy.Path (AbsDir, Path, dir, (</>))
import Pathy.Sandboxed (SandboxedPath, sandbox, sandboxAny, (<///>))
import Test.Assert (assertEqual)
import Test.Spec (Spec, it)
import Type.Prelude (Proxy(..))

prepare :: SandboxedPath Dir -> Aff Unit
prepare outerTmpDir = do
  A.rm'_dir outerTmpDir (A.rmOptionsDefault { recursive = true, force = true })
  A.mkdir' outerTmpDir { recursive: true, mode: permsAll }
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "1.txt")) "1"
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "2.txt")) "2"
  A.mkdir (outerTmpDir <///> (Proxy :: _ "dir1"))
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "3.txt")) "3"
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "4.txt")) "4"

-- test1 :: forall relOrAbs . IsRelOrAbs relOrAbs => PathyFS.HasPath (PathyFS.Dir relOrAbs) relOrAbs => SandboxedPath Dir -> Aff Unit
test1 :: SandboxedPath Dir -> Aff Unit
test1 outerTmpDir = do
  let outerTmpDirPrinted = printPath currentPrinter outerTmpDir
  logShow $ printPath currentPrinter outerTmpDir
  dir <- A.opendir' outerTmpDir (A.opendirOptionsDefault { recursive = true })
  let (relPath :: AbsDir) = PathyFS.path dir
  -- let (relPath :: RelDir) = PathyFS.path dir
  -- let (relPath :: P.Path P.Abs P.Dir) = PathyFS.path dir
  liftEffect $ log $ show relPath
  (files' :: Array (Dirent Abs)) <- entries dir
  liftEffect $ assertEqual
    { actual: show files' # String.replaceAll (Pattern outerTmpDirPrinted) (Replacement "$outerTmpDirPrinted$")
    , expected:
        """[Dirent {
  name: 'dir1',
  parentPath: '$outerTmpDirPrinted$',
  path: '$outerTmpDirPrinted$',
  [Symbol(type)]: 2
},Dirent {
  name: '1.txt',
  parentPath: '$outerTmpDirPrinted$',
  path: '$outerTmpDirPrinted$',
  [Symbol(type)]: 1
},Dirent {
  name: '2.txt',
  parentPath: '$outerTmpDirPrinted$',
  path: '$outerTmpDirPrinted$',
  [Symbol(type)]: 1
},Dirent {
  name: '3.txt',
  parentPath: '$outerTmpDirPrinted$dir1',
  path: '$outerTmpDirPrinted$dir1',
  [Symbol(type)]: 1
},Dirent {
  name: '4.txt',
  parentPath: '$outerTmpDirPrinted$dir1',
  path: '$outerTmpDirPrinted$dir1',
  [Symbol(type)]: 1
}]"""
    }
  case files' !! 0 of
    Nothing -> throwError $ error "no"
    Just file -> do
      let (relPath :: AbsDir) = PathyFS.parentPath file
      liftEffect $ log $ show relPath

  try (entries dir) >>= \(eitherFile :: Either Error (Array (PathyFS.Dirent Abs))) -> liftEffect $ assertEqual
    { actual: String.take 74 $ show eitherFile
    , expected: "(Left Error [ERR_DIR_CLOSED]: Directory handle was closed\n    at #readImpl"
    }
  try (entries dir) >>= \(eitherFile :: Either Error (Array (PathyFS.Dirent Abs))) -> liftEffect $ assertEqual
    { actual: String.take 74 $ show eitherFile
    , expected: "(Left Error [ERR_DIR_CLOSED]: Directory handle was closed\n    at #readImpl"
    }

sandboxOrThrow
  :: forall a b m
   . IsRelOrAbs a
  => IsDirOrFile b
  => MonadThrow Error m
  => Path Abs Dir
  -> Path a b
  -> m (SandboxedPath b)
sandboxOrThrow root path = sandbox root path # maybe (throwError $ error $ "cannot sandbox path: root = " <> show root <> ", path = " <> show path) pure

spec :: Spec Unit
spec = do
  it "test1" do
    cwd <- liftEffect PathyFS.cwd
    logShow $ printPath currentPrinter (sandboxAny cwd)
    -- logShow $ debugPrintPath currentPrinter cwd
    -- (outerTmpDir :: SandboxedPath Dir) <- sandboxOrThrow cwd (currentDir </> dir (Proxy :: _ "tmp") </> dir (Proxy :: _ "dir-entries-test"))
    -- (outerTmpDir :: SandboxedPath Dir) <- sandboxOrThrow cwd (currentDir </> dir (Proxy :: _ "tmp") </> dir (Proxy :: _ "dir-entries-test"))
    let (outerTmpDir :: SandboxedPath Dir) = sandboxAny (cwd </> dir (Proxy :: _ "tmp") </> dir (Proxy :: _ "dir-entries-test"))
    logShow $ printPath currentPrinter outerTmpDir
    prepare outerTmpDir
    test1 outerTmpDir
