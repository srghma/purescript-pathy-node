module Pathy.Node.FS.Stream where

import Effect (Effect)
import Node.FS.Stream (WriteStreamOptions, ReadStreamOptions)
import Node.FS.Stream as F
import Node.Stream (Readable, Writable)
import Pathy (File, SandboxedPath, printPath)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentPrinter)
import Prim.Row as Row

moduleName :: String
moduleName = "Pathy.Node.FS.Stream"

createWriteStream :: SandboxedPath File -> Effect (Writable ())
createWriteStream path = F.createWriteStream (printPath currentPrinter path)

createWriteStream'
  :: forall r trash
   . Row.Union r trash WriteStreamOptions
  => SandboxedPath File
  -> { | r }
  -> Effect (Writable ())
createWriteStream' path = F.createWriteStream' (printPath currentPrinter path)

createReadStream :: SandboxedPath File -> Effect (Readable ())
createReadStream path = F.createReadStream (printPath currentPrinter path)

createReadStream'
  :: forall r trash
   . Row.Union r trash ReadStreamOptions
  => SandboxedPath File
  -> { | r }
  -> Effect (Readable ())
createReadStream' path = F.createReadStream' (printPath currentPrinter path)
