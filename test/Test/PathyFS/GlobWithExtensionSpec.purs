module Test.PathyFS.GlobWithExtensionSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Pathy (printPath)
import Pathy.Node.OS.CurrentParserPrinter (currentPrinter)
import Pathy.Options.Applicative (globWithExtension')
import Pathy.Sandboxed (sandboxAny_AnyFilePathVariant)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

itParsesTo :: String -> Maybe String -> Spec Unit
itParsesTo path expected = it (path <> " -> " <> show expected) do
  (map print $ globWithExtension' "purs" path) `shouldEqual` expected
  where
  print = sandboxAny_AnyFilePathVariant >>> printPath currentPrinter

spec :: Spec Unit
spec = do
  itParsesTo "foo" (Just "/foo/**/*.purs")
  itParsesTo "foo/" (Just "/foo/**/*.purs")
  itParsesTo "foo.purs" (Just "/foo.purs")
  itParsesTo "foo.purs/" Nothing
  itParsesTo "foo/**" (Just "/foo/**/*.purs")
  itParsesTo "foo/**/*.purs" (Just "/foo/**/*.purs")
