module Test.PathyFS.GlobSpec where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Options (rmOptionsDefault) as A
import Node.FS.Perms (permsAll)
import Pathy (Dir, File, printPath)
import Pathy.Node.FS.Aff (cpDir, cpFile, globDirent, mkdir, mkdir', rm'_dir, writeTextFile) as A
import Pathy.Node.FS.Dirent as PathyFS
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentPrinter)
import Pathy.Node.Process (cwd) as PathyFS
import Pathy.Path (dir, (</>))
import Pathy.Sandboxed (SandboxedPath, sandboxAny, (<///>))
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

direntInfo
  :: PathyFS.Dirent
  -> { parentPath :: SandboxedPath Dir
     , type :: PathyFS.DirentType
     , name :: String
     }
direntInfo dirent =
  { parentPath: sandboxAny $ PathyFS.parentPath dirent
  , type: PathyFS.getType dirent
  , name: PathyFS.name dirent
  }

test1 :: SandboxedPath Dir -> Aff Unit
test1 outerTmpDir = do
  let (globbed :: SandboxedPath File) = outerTmpDir <///> (Proxy :: _ "**") <///> (Proxy :: _ "dir*")
  (res :: Array PathyFS.Dirent) <- A.globDirent [ globbed ]

  -- traceM $ show $ map direntInfo res
  -- traceM $ res

  liftEffect $ assertEqual
    { actual: map direntInfo res
    , expected:
        [ { parentPath: outerTmpDir
          , type: PathyFS.DirentType_Directory
          , name: "dir1"
          }
        , { parentPath: outerTmpDir
          , type: PathyFS.DirentType_Directory
          , name: "dir2"
          }
        ]
    }
  pure unit

test2 :: SandboxedPath Dir -> Aff Unit
test2 outerTmpDir = do
  let (globbed :: SandboxedPath File) = outerTmpDir <///> (Proxy :: _ "**") <///> (Proxy :: _ "*.js")
  (res :: Array PathyFS.Dirent) <- A.globDirent [ globbed ]
  liftEffect $ assertEqual
    { actual: map direntInfo res
    , expected:
        [ { parentPath: outerTmpDir
          , type: PathyFS.DirentType_File
          , name: "1.js"
          }
        , { parentPath: outerTmpDir <///> (Proxy :: _ "dir2")
          , type: PathyFS.DirentType_File
          , name: "4.js"
          }
        , { parentPath: outerTmpDir <///> (Proxy :: _ "dir2")
          , type: PathyFS.DirentType_File
          , name: "5.js"
          }
        , { parentPath: outerTmpDir <///> (Proxy :: _ "dir1")
          , type: PathyFS.DirentType_File
          , name: "4.js"
          }
        , { parentPath: outerTmpDir <///> (Proxy :: _ "dir1")
          , type: PathyFS.DirentType_File
          , name: "5.js"
          }
        ]
    }
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
    test2 outerTmpDir
