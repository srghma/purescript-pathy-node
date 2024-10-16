module Pathy.Node.OS.CurrentParserPrinter where

import Pathy.Parser (Parser, posixParser)
import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.OS (type_)
import Pathy (Printer, posixPrinter, windowsPrinter)

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
