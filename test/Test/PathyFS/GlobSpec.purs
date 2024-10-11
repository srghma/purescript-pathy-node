module Test.PathyFS.GlobSpec where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array ((!!))
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Node.Encoding (Encoding(..))
import Node.FS.Options (opendirOptionsDefault, rmOptionsDefault) as A
import Node.FS.Perms (permsAll)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, File, AbsAnyPathVariant, file, printPath)
import Pathy.Node.FS.Aff as A
import Pathy.Node.FS.Aff.Dir (entries)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentPrinter)
import Pathy.Node.Process as PathyFS
import Pathy.Path (AbsDir, Path, dir, (</>))
import Pathy.Sandboxed (SandboxedPath, sandbox, sandboxAny, (<///>))
import Test.Assert (assertEqual)
import Test.Spec (Spec, it)
import Type.Prelude (Proxy(..))

prepare :: SandboxedPath Dir -> Aff Unit
prepare outerTmpDir = do
  A.rm'_dir outerTmpDir (A.rmOptionsDefault { recursive = true, force = true })
  A.mkdir' outerTmpDir { recursive: true, mode: permsAll }
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "1.js")) "1"
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "2.txt")) "2"
  A.mkdir (outerTmpDir <///> (Proxy :: _ "dir1"))
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "3.txt")) "3"
  A.writeTextFile UTF8 (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "4.js")) "4"
  A.cpFile (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "4.js") :: SandboxedPath File) (outerTmpDir <///> (Proxy :: _ "dir1") <///> (Proxy :: _ "5.js"))
  A.cpDir (outerTmpDir <///> (Proxy :: _ "dir1") :: SandboxedPath Dir) (outerTmpDir <///> (Proxy :: _ "dir2"))

-- test1 :: forall relOrAbs . IsRelOrAbs relOrAbs => PathyFS.HasPath (PathyFS.Dir relOrAbs) relOrAbs => SandboxedPath Dir -> Aff Unit
test1 :: SandboxedPath Dir -> Aff Unit
test1 outerTmpDir = do
  let (globbed :: SandboxedPath File) = outerTmpDir <///> (Proxy :: _ "**") <///> (Proxy :: _ "dir*")
  -- let (globbed :: SandboxedPath File) = outerTmpDir <///> (Proxy :: _ "**") <///> (Proxy :: _ "*.js")
  (res :: Array AbsAnyPathVariant) <- A.glob [globbed]
  traceM res
  pure unit

spec :: Spec Unit
spec = do
  it "test1" do
    cwd <- liftEffect PathyFS.cwd
    logShow $ printPath currentPrinter (sandboxAny cwd)
    let (outerTmpDir :: SandboxedPath Dir) = sandboxAny (cwd </> dir (Proxy :: _ "tmp") </> dir (Proxy :: _ "dir-entries-test"))
    logShow $ printPath currentPrinter outerTmpDir
    prepare outerTmpDir
    test1 outerTmpDir
