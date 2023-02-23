module Data.Conversion.Problem.Term
  ( Term (..),
    parseVariable,
    parseTerm,
    parseFunSymbol,
    foldTerm,
    termFunArities,
    checkConsistentSig,
  )
where

import Data.Conversion.Problem.Term.Parse
import Data.Conversion.Problem.Term.Type
