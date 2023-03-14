-- |
-- Module      : Test.Parse.Utils
-- Description : Parsing utils for testing
--
-- This module defines reusable helpers for testing @Megaparsec@ parsers.
module Test.Parse.Utils
  ( assertFailParseList,
    assertParseList,
  )
where

import Data.Conversion.Parse.Utils (Parser)
import Data.Either (isLeft)
import Data.Text (Text, pack)
import Data.Void (Void)
import Test.HUnit
import Text.Megaparsec (anySingle, eof, errorBundlePretty, many, parse, (<|>))
import Text.Megaparsec.Error (ParseErrorBundle)

-- | Helper function to apply parser @p@ to string @xs@ and return either
-- @Left err@ if parsing fails or or @Right result@ if parsing succeeds.
--
-- __'parseFromString' requires the parser to consume the entire input__ (until @eof@).
-- This can result in different behaviour to if the parser were allowed to consume only part of the input.
parseFromString :: String -> Parser a -> Either (ParseErrorBundle Text Void) a
parseFromString xs p =
  parse
    (p <* (eof <|> displayUnconsumed))
    ""
    (pack xs)
  where
    displayUnconsumed :: Parser b
    displayUnconsumed = do
      unconsumed <- many anySingle
      fail ("Input " ++ show xs ++ " was not completely consumed. Got remainder " ++ show unconsumed)

-- | Assert that parsing string @xs@ using parser @p@ fails.
-- If parsing succeeds, then an error will be shown.
assertParseFail :: Show a => String -> Parser a -> Assertion
assertParseFail xs p = assertBool (show xs ++ " should not be parseable. Got: " ++ show res) (isLeft res)
  where
    res = parseFromString xs p

-- | Assert that parsing string @xs@ with parser @p@ returns result @expected@.
-- If parsing fails or a different result is obtained then an error will be shown.
assertParse :: (Eq a, Show a) => String -> Parser a -> a -> Assertion
assertParse xs p expected = case parseFromString xs p of
  Left err -> assertFailure (errorBundlePretty err)
  Right res -> assertEqual (show xs ++ " not parsed correctly") expected res

-- | Assert that parsing succeeds for each element of @xs@ using parser @p@.
-- Each element of @xs@ should be of the form @(stringToParse, expectedAnswer)@.
--
-- An error and the label @l@ will be shown if the parsing fails or if an unexpected result is obtained.
assertParseList :: (Eq a, Show a) => String -> [(String, a)] -> Parser a -> Test
assertParseList l xs p = TestList [TestLabel l $ TestCase (assertParse val p expected) | (val, expected) <- xs]

-- | Assert that parsing fails using parser @p@ on each string in list @xs@ using 'assertParseFail'.
-- An error will be shown for each element of @xs@ for which parsing succeeds.
--
-- If parsing fails, label @l@ will be shown with the error message. It is recommended to use a descriptive label to aid debugging.
assertFailParseList :: Show a => String -> [String] -> Parser a -> Test
assertFailParseList l xs p = TestList [TestLabel l $ TestCase (assertParseFail val p) | val <- xs]