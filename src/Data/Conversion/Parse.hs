module Data.Conversion.Parse
  ( exampleParse,
  )
where

import Data.Conversion.Parse.COPS

exampleParse :: IO (Problem String String)
exampleParse = parseIO exampleTrs

exampleTrs :: String
exampleTrs = "(VAR x y) (RULES  +(x,0) -> x \n +(x,s(y)) -> s(+(x,y)))"