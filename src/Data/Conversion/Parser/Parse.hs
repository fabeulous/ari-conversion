module Data.Conversion.Parser.Parse
  ( exampleParse,
  )
where

import Data.Conversion.Problem.Trs.Trs

exampleParse :: IO (Trs String String)
exampleParse = undefined-- parseIO exampleTrs

exampleTrs :: String
exampleTrs = "(VAR x y) (RULES  +(x,0) -> x \n +(x,s(y)) -> s(+(x,y)))"