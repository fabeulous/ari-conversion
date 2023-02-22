module Main (main) where

import Data.Conversion.Problem.Rule (Rule (..))
import Data.Conversion.Problem.Term (Term (..), parseTerm)
import Data.Conversion.Parser.Parse (exampleParse)
import Prettyprinter
import Text.Parsec hiding (parse)


-- | Parse a term from a string input where @xs@ is a list of variables
fromString :: [String] -> String -> Either ParseError (Term String String)
fromString xs = undefined -- runP (parseTerm xs) () ""



main :: IO ()
main = do
  let t1 = Fun 'e' [Var 3, Fun 'f' []]
  let t2 = Fun 'g' [Fun 'f' [Var 1, Var 2], Var 9] 
  let t3 = Fun "+" [Var "x",Fun "s" [Var "y"]]
  let t4 = Fun "s" [Fun "+" [Var "x",Var "y"]]
  let r = Rule {lhs = t3, rhs = t4}
  print r
  prob <- exampleParse
  print prob
  print (pretty t3)
  print (pretty r)
  print ""
  print (fromString ["x", "y"] "f(x))")
  print (fromString ["x", "y"] "f x")
