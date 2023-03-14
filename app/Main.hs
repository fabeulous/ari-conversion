module Main (main) where

import Data.Conversion.Parse.ParseTrs (parseCopsTrs)
import Data.Conversion.Problem.Trs.Trs (Trs)
import Data.Conversion.Unparse.UnparseTrs (unparseAriTrs)
import Data.Text (pack)
import Text.Megaparsec (errorBundlePretty, parse)

-- | @trs-conversion-exe@ entry point. Can be run by calling
-- > stack build
-- > stack exec trs-conversion-exe
main :: IO ()
main = do
  putStrLn $ "Parsing example TRS: " ++ input ++ "\n"
  trs <- parseCops input
  putStrLn $ "Got result: " ++ show trs ++ "\n"
  case unparseAriTrs trs of
    Left err -> ioError $ userError err
    Right ariTrs -> print ariTrs
  where
    input = "(VAR x y)(RULES  f(x,y) -> g(c))(COMMENT A simple example)"

-- | Run 'parseCopsTrs' on an input string and return the resulting 'Trs'
parseCops :: String -> IO (Trs String String)
parseCops input = case parse parseCopsTrs "COPS Example" (pack input) of
  Left err -> ioError $ userError (errorBundlePretty err)
  Right trs -> return trs