module Test.Parse.Utils
  ( -- qqjf module description
    assertFailParseList,
    assertParseList,
  )
where

import Data.Conversion.Parser.Parse.Utils (Parser)
import Data.Either (isLeft)
import Data.Text (Text, pack)
import Data.Void (Void)
import Test.HUnit
import Text.Megaparsec (eof, errorBundlePretty, parse)
import Text.Megaparsec.Error (ParseErrorBundle)

-- | Parse from a string until end of input qqjf
parseFromString :: String -> Parser a -> Either (ParseErrorBundle Text Void) a
parseFromString xs p = parse (p <* eof) "" (pack xs)

-- | Assert that parsing string @xs@ using parser @p@ fails qqjf
assertParseFail :: Show a => String -> Parser a -> Assertion
assertParseFail xs p = assertBool (xs ++ " should not be parseable. Got: " ++ show res) (isLeft res)
  where
    res = parseFromString xs p

-- | Assert that parsing string @xs@ with parser @p@ returns result @expected@
assertParse :: (Eq a, Show a) => String -> Parser a -> a -> Assertion
assertParse xs p expected = case parseFromString xs p of
  Left err -> assertFailure (errorBundlePretty err)
  Right res -> assertEqual (xs ++ " not parsed correctly") expected res

-- | Assert that a list of strings with expected answers all parse qqjf
assertParseList :: (Eq a, Show a) => [(String, a)] -> Parser a -> Test
assertParseList xs p = TestList [TestCase (assertParse val p expected) | (val, expected) <- xs]

-- | Assert qqjf
assertFailParseList :: Show a => [String] -> Parser a -> Test
assertFailParseList xs p = TestList [TestCase (assertParseFail val p) | val <- xs]