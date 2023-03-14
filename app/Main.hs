module Main (main) where

import Data.Conversion.Parse.ParseMsTrs (parseAriMsTrs)
import Data.Conversion.Parse.ParseTrs (parseCopsTrs)
import Data.Conversion.Problem.MsTrs.MsTrs (MsTrs)
import Data.Conversion.Problem.Trs.Trs (Trs)
import Data.Conversion.Unparse.UnparseMsTrs (unparseCopsMsTrs)
import Data.Conversion.Unparse.UnparseTrs (unparseAriTrs)
import Data.Text (pack)
import Prettyprinter (Doc)
import Text.Megaparsec (eof, errorBundlePretty, parse)

-- | @trs-conversion-exe@ entry point. Can be run by calling
-- > stack build
-- > stack exec trs-conversion-exe
--
-- Currently just contains some simple hard-coded examples to illustrate
-- parsing and unparsing functionality.
main :: IO ()
main = do
  -- Parse and unparse inputTrs
  putStrLn $ "Parsing example TRS:\n" ++ inputTrs ++ "\n"
  trs <- parseCops inputTrs
  putStrLn $ "Got result:\n" ++ show trs ++ "\n"
  ariTrs <- unparseIo unparseAriTrs trs
  putStrLn $ "In ARI format:\n" ++ show ariTrs
  putStrLn "\n--------\n"
  -- Parse and unparse inputMsTrs
  putStrLn $ "Parsing example MSTRS:\n" ++ inputMsTrs ++ "\n"
  mstrs <- parseAri inputMsTrs
  putStrLn $ "Got result:\n" ++ show mstrs ++ "\n"
  copsMsTrs <- unparseIo unparseCopsMsTrs mstrs
  putStrLn $ "In COPS format:\n" ++ show copsMsTrs
  where
    inputTrs = "(VAR x y)\n(RULES  f(x,y) -> g(c))\n(COMMENT A simple example)"
    inputMsTrs =
      "(format MSTRS)\n\
      \(sort List)\n\
      \(sort Nat)\n\
      \(fun app :sort (List List List))\n\
      \(fun cons :sort (Nat List List))\n\
      \(fun nil :sort (List))\n\
      \(rule (app nil ys) ys)\n\
      \(rule (app (cons x xs) ys) (cons x (app xs ys)))"

-- | Run 'parseCopsTrs' on an input string and return the resulting 'Trs'
parseCops :: String -> IO (Trs String String)
parseCops input = case parse (parseCopsTrs <* eof) "COPS TRS Example" (pack input) of
  Left err -> ioError $ userError (errorBundlePretty err)
  Right trs -> return trs

-- | Run 'parseAriMsTrs' on an input string and return the resulting 'MsTrs'
parseAri :: String -> IO (MsTrs String String String)
parseAri input = case parse (parseAriMsTrs <* eof) "ARI MSTRS Example" (pack input) of
  Left err -> ioError $ userError (errorBundlePretty err)
  Right mstrs -> return mstrs

-- | Takes an unparsing function @up@ as an argument and wraps the result in the IO monad
unparseIo :: (a -> Either String (Doc a)) -> a -> IO (Doc a)
unparseIo up input = case up input of
  Left err -> ioError $ userError err
  Right copsMsTrs -> return copsMsTrs
