module Pathy.Node.Internal.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Node.Path (FilePath)
import Pathy (AbsAnyPathVariant, AnyAnyPathVariant, Parser, Path, RelAnyPathVariant)
import Pathy.Node.OS.Internal.CurrentParserPrinter (currentParser)
import Pathy.Node.OS.Internal.Utils as Internal
import Pathy.Parser (parseAbsAnyPathVariant, parseAbsDir, parseAbsFile, parseAnyAnyPathVariant, parseRelAnyPathVariant, parseRelDir, parseRelFile) as Pathy
import Pathy.Phantom (Abs, Dir, File, Rel, RelOrAbs)
import Type.Prelude (Proxy(..))

ensureTrailingSlash :: String -> String
ensureTrailingSlash s =
  case String.charAt (String.length s - 1) s of
    Just '/' -> s
    _ -> s <> "/"

data EnsureTrailingSlash
  = EnsureTrailingSlash_Always
  | EnsureTrailingSlash_No
  -- | EnsureTrailingSlash_OnlyIfGettingStatsWillTellThatItIsReallyADear

class ParseToPathVariantOrThrow_KnownOutputType :: Symbol -> Type -> Constraint
class ParseToPathVariantOrThrow_KnownOutputType symbol variant | symbol -> variant where
  parseToPathVariantOrThrow_data :: Proxy symbol -> { parse :: Parser -> String -> Maybe variant, outputType :: String, ensureTrailingSlash :: EnsureTrailingSlash }

-- why Proxy?
-- impossible to match `Variant (...)` - no meaningful exists to match Row

instance ParseToPathVariantOrThrow_KnownOutputType "AnyAnyPathVariant" AnyAnyPathVariant where
  parseToPathVariantOrThrow_data _ = { parse: Pathy.parseAnyAnyPathVariant, outputType: "AnyAnyPathVariant", ensureTrailingSlash: EnsureTrailingSlash_No }
else instance ParseToPathVariantOrThrow_KnownOutputType "AbsAnyPathVariant" AbsAnyPathVariant where
  parseToPathVariantOrThrow_data _ = { parse: Pathy.parseAbsAnyPathVariant, outputType: "AbsAnyPathVariant", ensureTrailingSlash: EnsureTrailingSlash_No }
else instance ParseToPathVariantOrThrow_KnownOutputType "RelAnyPathVariant" RelAnyPathVariant where
  parseToPathVariantOrThrow_data _ = { parse: Pathy.parseRelAnyPathVariant, outputType: "RelAnyPathVariant", ensureTrailingSlash: EnsureTrailingSlash_No }
else instance ParseToPathVariantOrThrow_KnownOutputType "Path Rel Dir" (Path Rel Dir) where
  parseToPathVariantOrThrow_data _ = { parse: Pathy.parseRelDir, outputType: "Path Rel Dir", ensureTrailingSlash: EnsureTrailingSlash_Always }
else instance ParseToPathVariantOrThrow_KnownOutputType "Path Rel File" (Path Rel File) where
  parseToPathVariantOrThrow_data _ = { parse: Pathy.parseRelFile, outputType: "Path Rel File", ensureTrailingSlash: EnsureTrailingSlash_No }
else instance ParseToPathVariantOrThrow_KnownOutputType "Path Abs Dir" (Path Abs Dir) where
  parseToPathVariantOrThrow_data _ = { parse: Pathy.parseAbsDir, outputType: "Path Abs Dir", ensureTrailingSlash: EnsureTrailingSlash_Always }
else instance ParseToPathVariantOrThrow_KnownOutputType "Path Abs File" (Path Abs File) where
  parseToPathVariantOrThrow_data _ = { parse: Pathy.parseAbsFile, outputType: "Path Abs File", ensureTrailingSlash: EnsureTrailingSlash_No }

class AnyDirToVariant :: Symbol -> RelOrAbs -> Type -> Constraint
class ParseToPathVariantOrThrow_KnownOutputType symbol variant <= AnyDirToVariant symbol rel variant | rel -> variant, symbol -> variant where
  anyDirToVariant_proxy :: Proxy symbol

instance AnyDirToVariant "RelAnyPathVariant" Rel RelAnyPathVariant where
  anyDirToVariant_proxy = (Proxy :: Proxy "RelAnyPathVariant")
else instance AnyDirToVariant "AbsAnyPathVariant" Abs AbsAnyPathVariant where
  anyDirToVariant_proxy = (Proxy :: Proxy "AbsAnyPathVariant")
else instance AnyDirToVariant "Path Rel Dir" Abs (Path Rel Dir) where
  anyDirToVariant_proxy = (Proxy :: Proxy "Path Rel Dir")
else instance AnyDirToVariant "Path Rel File" Rel (Path Rel File) where
  anyDirToVariant_proxy = (Proxy :: Proxy "Path Rel File")
else instance AnyDirToVariant "Path Abs Dir" Rel (Path Abs Dir) where
  anyDirToVariant_proxy = (Proxy :: Proxy "Path Abs Dir")
else instance AnyDirToVariant "Path Abs File" Abs (Path Abs File) where
  anyDirToVariant_proxy = (Proxy :: Proxy "Path Abs File")

class EnsureTrailingSlashM m where
  ensureTrailingSlashM :: EnsureTrailingSlash -> String -> m String

instance EnsureTrailingSlashM Aff where
  ensureTrailingSlashM :: EnsureTrailingSlash -> String -> Aff String
  ensureTrailingSlashM EnsureTrailingSlash_No s = pure s
  ensureTrailingSlashM EnsureTrailingSlash_Always s = pure $ ensureTrailingSlash s

instance EnsureTrailingSlashM Effect where
  ensureTrailingSlashM :: EnsureTrailingSlash -> String -> Effect String
  ensureTrailingSlashM EnsureTrailingSlash_No s = pure s
  ensureTrailingSlashM EnsureTrailingSlash_Always s = pure $ ensureTrailingSlash s

parsePathOrThrow
  :: forall symbol variant m
   . MonadThrow Error m
  => EnsureTrailingSlashM m
  => ParseToPathVariantOrThrow_KnownOutputType symbol variant
  => Proxy symbol
  -> { moduleName :: String
     , functionName :: String
     , filePath :: FilePath
     }
  -> m variant
parsePathOrThrow p { moduleName, functionName, filePath } = do
  let { parse, outputType, ensureTrailingSlash } = parseToPathVariantOrThrow_data p
  filePath' <- ensureTrailingSlashM ensureTrailingSlash filePath
  Internal.parsePathOrThrow
    { parse
    , parser: currentParser
    , moduleName
    , functionName
    , outputType
    , filePath: filePath'
    }
