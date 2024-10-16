module Pathy.OptParse where

import Pathy (AbsAnyPathVariant, AbsDir, AbsFile, AnyAnyPathVariant, AnyDirPathVariant, AnyFilePathVariant, RelAnyPathVariant, RelDir, RelFile)
import Prelude

import Data.Either as Either
import Options.Applicative (ReadM, eitherReader)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser)
import Pathy.Parser as Pathy
import Pathy.Node.OS.Internal.EnsureTrailingSlash (ensureTrailingSlash)

-- | Attempts to parse a relative file.
parseRelFile :: ReadM RelFile
parseRelFile = eitherReader $ Either.note "Invalid relative file path" <<< Pathy.parseRelFile currentParser

-- | Attempts to parse an absolute file.
parseAbsFile :: ReadM AbsFile
parseAbsFile = eitherReader $ Either.note "Invalid absolute file path" <<< Pathy.parseAbsFile currentParser

-- | Attempts to parse a relative directory.
parseRelDir :: ReadM RelDir
parseRelDir = eitherReader $ Either.note "Invalid relative directory path" <<< Pathy.parseRelDir currentParser <<< ensureTrailingSlash

-- | Attempts to parse an absolute directory.
parseAbsDir :: ReadM AbsDir
parseAbsDir = eitherReader $ Either.note "Invalid absolute directory path" <<< Pathy.parseAbsDir currentParser <<< ensureTrailingSlash

-- | Attempts to parse an absolute or relative directory.
parseAnyDirPathVariant :: ReadM AnyDirPathVariant
parseAnyDirPathVariant = eitherReader $ Either.note "Invalid directory path" <<< Pathy.parseAnyDirPathVariant currentParser <<< ensureTrailingSlash

-- | Attempts to parse an absolute or relative file.
parseAnyFilePathVariant :: ReadM AnyFilePathVariant
parseAnyFilePathVariant = eitherReader $ Either.note "Invalid file path" <<< Pathy.parseAnyFilePathVariant currentParser

-- | Attempts to parse a relative directory or file.
-- | NOTE: this function will parse "mypath" as file and "mypath/" as dir, be careful !!!
parseRelAnyPathVariant :: ReadM RelAnyPathVariant
parseRelAnyPathVariant = eitherReader $ Either.note "Invalid relative path" <<< Pathy.parseRelAnyPathVariant currentParser

-- | Attempts to parse an absolute directory or file.
-- | NOTE: this function will parse "mypath" as file and "mypath/" as dir, be careful !!!
parseAbsAnyPathVariant :: ReadM AbsAnyPathVariant
parseAbsAnyPathVariant = eitherReader $ Either.note "Invalid absolute path" <<< Pathy.parseAbsAnyPathVariant currentParser

-- | Attempts to parse an absolute or relative directory or file.
-- | NOTE: this function will parse "mypath" as file and "mypath/" as dir, be careful !!!
parseAnyAnyPathVariant :: ReadM AnyAnyPathVariant
parseAnyAnyPathVariant = eitherReader $ Either.note "Invalid path" <<< Pathy.parseAnyAnyPathVariant currentParser
