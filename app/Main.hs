module Main (main) where

import Data.Conversion.Rule (Rule (..))
import Data.Conversion.Term (Term (..))
import Data.Conversion.Parse (exampleParse)
import Data.Conversion.Unparse (unparseTrsAri)

main :: IO ()
main = do
  let t1 = Fun 'e' [Var 3, Fun 'f' []]
  let t2 = Fun 'g' [Fun 'f' [Var 1, Var 2], Var 9]
  let r = Rule {lhs = t1, rhs = t2}
  print r
  prob <- exampleParse
  print prob
  print (unparseTrsAri prob)
