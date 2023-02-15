module Main (main) where

import Data.Conversion.Problem.Rule (Rule (..))
import Data.Conversion.Problem.Term (Term (..))
import Data.Conversion.Parser.Parse (exampleParse)
import Prettyprinter

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
