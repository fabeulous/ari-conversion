module Data.Conversion.Parser.Parse
  ( exampleParse,
  )
where

import Data.Conversion.Problem.TRS 
import Data.Conversion.Parser.Parse.COPS

exampleParse :: IO (TRS String String)
exampleParse = parseIO exampleTrs

exampleTrs :: String
exampleTrs = "(VAR x y) (RULES  +(x,0) -> x \n +(x,s(y)) -> s(+(x,y)))"