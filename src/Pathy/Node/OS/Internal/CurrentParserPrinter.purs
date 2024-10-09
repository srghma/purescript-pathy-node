module Pathy.Node.OS.Internal.CurrentParserPrinter where

import Pathy.Parser
import Pathy.Path
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Node.OS (type_)
import Node.OS as OS
import Node.Path (FilePath)
import Pathy (Abs, AbsAnyPathVariant, Dir, Parser, Printer, Rel, posixParser, posixPrinter, windowsPrinter)
import Pathy.Parser as P
import Pathy.Phantom (RelOrAbs)
import Type.Prelude (Proxy(..))

data OS = Windows | Posix

getOS :: Effect OS
getOS = do
  os <- type_
  pure case os of
    "Windows_NT" -> Windows
    _ -> Posix

currentPrinter :: Printer
currentPrinter = unsafePerformEffect $
  getOS <#>
    case _ of
      Windows -> windowsPrinter
      Posix -> posixPrinter

currentParser :: Parser
currentParser = unsafePerformEffect $
  getOS <#>
    case _ of
      Windows -> posixParser -- windowsParser
      Posix -> posixParser
