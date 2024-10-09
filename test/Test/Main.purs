module Test.Main where

import Node.FS.Aff
import Pathy
import Pathy.Path
import Pathy.Sandboxed
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array ((!!))
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Node.Encoding (Encoding(..))
import Node.FS.Options as A
import Node.FS.Perms (permsAll)
import Node.Path (FilePath)
import Node.Path as Path
import Pathy as P
import Pathy.Node.FS.Aff as A
import Pathy.Node.FS.Aff as PathyFs
import Pathy.Node.FS.Aff.Dir (entries)
import Pathy.Node.FS.Dir as PathyFS
import Pathy.Node.FS.Dir as PathyFs
import Pathy.Node.FS.Dirent (Dirent)
import Pathy.Node.FS.Dirent as PathyFs
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentPrinter)
import Pathy.Node.Process as PathyFS
import Test.Assert (assertEqual)
import Type.Prelude (Proxy(..))

prepare :: forall relOrAbs . IsRelOrAbs relOrAbs => SandboxedPath relOrAbs Dir -> Aff Unit
prepare outerTmpDir = do
  A.rm'_dir outerTmpDir (A.rmOptionsDefault { recursive = true, force = true })
  A.mkdir' outerTmpDir { recursive: true, mode: permsAll }
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "1.txt")) "1"
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "2.txt")) "2"
  A.mkdir (outerTmpDir <///> (Proxy :: _ "dir1"))
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "3.txt")) "3"
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "4.txt")) "4"


test1 :: forall relOrAbs . IsRelOrAbs relOrAbs => PathyFS.HasPath (PathyFS.Dir relOrAbs) relOrAbs => SandboxedPath relOrAbs Dir -> Aff Unit
test1 outerTmpDir = do
  logShow $ printPath currentPrinter outerTmpDir
  dir <- A.opendir' outerTmpDir (A.opendirOptionsDefault { recursive = true })
  -- let (relPath :: RelDir) = PathyFs.path dir
  let (relPath :: P.Path relOrAbs P.Dir) = PathyFs.path dir
  pure unit
--   liftEffect $ log $ show relPath
--   (files' :: Array (Dirent Rel)) <- entries dir
--   liftEffect $ assertEqual
--     { actual: show files'
--     , expected:
--         """[Dirent Dirent {
--   name: 'dir1',
--   parentPath: './tmp/dir-entries-test/',
--   path: './tmp/dir-entries-test/',
--   [Symbol(type)]: 2
-- },Dirent Dirent {
--   name: '1.txt',
--   parentPath: './tmp/dir-entries-test/',
--   path: './tmp/dir-entries-test/',
--   [Symbol(type)]: 1
-- },Dirent Dirent {
--   name: '2.txt',
--   parentPath: './tmp/dir-entries-test/',
--   path: './tmp/dir-entries-test/',
--   [Symbol(type)]: 1
-- },Dirent Dirent {
--   name: '3.txt',
--   parentPath: 'tmp/dir-entries-test/dir1',
--   path: 'tmp/dir-entries-test/dir1',
--   [Symbol(type)]: 1
-- },Dirent Dirent {
--   name: '4.txt',
--   parentPath: 'tmp/dir-entries-test/dir1',
--   path: 'tmp/dir-entries-test/dir1',
--   [Symbol(type)]: 1
-- }]"""
--     }
--   case files' !! 0 of
--     Nothing -> throwError $ error "no"
--     Just file -> do
--       let (relPath :: RelDir) = PathyFs.parentPath file
--       liftEffect $ log $ show relPath
--
--   -- try (entries dir) >>= \(eitherFile :: Either Error (Array (Dirent String))) -> liftEffect $ assertEqual
--   --   { actual: String.take 74 $ show eitherFile
--   --   , expected: "(Left Error [ERR_DIR_CLOSED]: Directory handle was closed\n    at #readImpl"
--   --   }
--   -- try (entries dir) >>= \(eitherFile :: Either Error (Array (Dirent String))) -> liftEffect $ assertEqual
--   --   { actual: String.take 74 $ show eitherFile
--   --   , expected: "(Left Error [ERR_DIR_CLOSED]: Directory handle was closed\n    at #readImpl"
--   --   }

sandboxOrThrow
  :: forall a b m
   . IsRelOrAbs a
  => IsDirOrFile b
  => MonadThrow Error m
  => Path Abs Dir
  -> Path a b
  -> m (SandboxedPath a b)
sandboxOrThrow root path = sandbox root path # maybe (throwError $ error $ "cannot sandbox path: root = " <> show root <> ", path = " <> show path) pure

main :: Effect Unit
main = launchAff_ do
  cwd <- liftEffect PathyFS.cwd
  -- logShow $ debugPrintPath currentPrinter cwd
  (outerTmpDir :: SandboxedPath Rel Dir) <- sandboxOrThrow cwd (currentDir </> dir (Proxy :: _ "tmp") </> dir (Proxy :: _ "dir-entries-test"))
  logShow $ printPath currentPrinter outerTmpDir
  prepare outerTmpDir
  test1 outerTmpDir
