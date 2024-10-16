module Pathy.Node.OS.Internal.ParsePathWithPrettyError where

import Pathy.Parser (Parser)
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..), note')
import Data.Maybe (Maybe)
import Data.String (joinWith)
import Effect.Exception (Error, error)
import Node.Path (FilePath)

parsePathEither
  :: forall x
   . { parse :: Parser -> String -> Maybe x
     , parser :: Parser
     , moduleName :: String
     , functionName :: String
     , outputType :: String
     , filePath :: FilePath
     }
  -> Either String x
parsePathEither { parse, parser, moduleName, functionName, outputType, filePath } =
  parse parser filePath # note' \_ ->
    joinWith ""
      [ moduleName
      , ": "
      , functionName
      , " -> cannot parse "
      , show outputType
      , " path "
      , show filePath
      ]

parsePathOrThrow
  :: forall x m
   . MonadThrow Error m
  => { parse :: Parser -> String -> Maybe x
     , parser :: Parser
     , moduleName :: String
     , functionName :: String
     , outputType :: String
     , filePath :: FilePath
     }
  -> m x
parsePathOrThrow options =
  case parsePathEither options of
    Left err -> throwError $ error err
    Right x -> pure x
