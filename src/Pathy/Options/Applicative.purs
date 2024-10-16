module Pathy.Options.Applicative where

import Prelude

import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String (Pattern(..))
import Data.String as String
import Node.Path as Path
import Options.Applicative (ReadM, eitherReader)
import Pathy (AbsAnyPathVariant, AbsDir, AbsFile, AnyAnyPathVariant, AnyDirPathVariant, AnyFilePathVariant, RelAnyPathVariant, RelDir, RelFile)
import Pathy.Node.OS.CurrentParserPrinter (currentParser)
import Pathy.Node.OS.Internal.EnsureTrailingSlash (ensureTrailingSlash)
import Pathy.Parser as Pathy

-- | Attempts to parse a relative file.
relFile :: ReadM RelFile
relFile = eitherReader $ Either.note "Invalid relative file path" <<< Pathy.parseRelFile currentParser

-- | Attempts to parse an absolute file.
absFile :: ReadM AbsFile
absFile = eitherReader $ Either.note "Invalid absolute file path" <<< Pathy.parseAbsFile currentParser

-- | Attempts to parse a relative directory.
relDir :: ReadM RelDir
relDir = eitherReader $ Either.note "Invalid relative directory path" <<< Pathy.parseRelDir currentParser <<< ensureTrailingSlash

-- | Attempts to parse an absolute directory.
absDir :: ReadM AbsDir
absDir = eitherReader $ Either.note "Invalid absolute directory path" <<< Pathy.parseAbsDir currentParser <<< ensureTrailingSlash

-- | Attempts to parse an absolute or relative directory.
anyDirPathVariant :: ReadM AnyDirPathVariant
anyDirPathVariant = eitherReader $ Either.note "Invalid directory path" <<< Pathy.parseAnyDirPathVariant currentParser <<< ensureTrailingSlash

-- | Attempts to parse an absolute or relative file.
anyFilePathVariant :: ReadM AnyFilePathVariant
anyFilePathVariant = eitherReader $ Either.note "Invalid file path" <<< Pathy.parseAnyFilePathVariant currentParser

-- | Attempts to parse a relative directory or file.
-- | NOTE: this function will parse "mypath" as file and "mypath/" as dir, be careful !!!
relAnyPathVariant :: ReadM RelAnyPathVariant
relAnyPathVariant = eitherReader $ Either.note "Invalid relative path" <<< Pathy.parseRelAnyPathVariant currentParser

-- | Attempts to parse an absolute directory or file.
-- | NOTE: this function will parse "mypath" as file and "mypath/" as dir, be careful !!!
absAnyPathVariant :: ReadM AbsAnyPathVariant
absAnyPathVariant = eitherReader $ Either.note "Invalid absolute path" <<< Pathy.parseAbsAnyPathVariant currentParser

-- | Attempts to parse an absolute or relative directory or file.
-- | NOTE: this function will parse "mypath" as file and "mypath/" as dir, be careful !!!
anyAnyPathVariant :: ReadM AnyAnyPathVariant
anyAnyPathVariant = eitherReader $ Either.note "Invalid path" <<< Pathy.parseAnyAnyPathVariant currentParser

------

globWithExtension' :: String -> String -> Maybe AnyFilePathVariant
globWithExtension' = \ext path -> Pathy.parseAnyFilePathVariant currentParser (dirToGlob ext path)
  where
  dirToGlob :: String -> String -> String
  dirToGlob ext path =
    if Path.extname path == "" then
      if Maybe.isJust (String.stripSuffix (Pattern "**") path) then
        Path.concat [ path, "*." <> ext ]
      else
        Path.concat [ path, "**", "*." <> ext ]
    else
      path

-- | Attempts to parse an absolute or relative file.
globWithExtension :: String -> ReadM AnyFilePathVariant
globWithExtension ext = eitherReader $ Either.note ("Invalid glob with extension " <> ext) <<< globWithExtension' ext
