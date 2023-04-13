{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.COPS.Term
Description : Term, function symbol, and variable parser

This module defines functions to parse terms from a @String@ input.
Also specifies the allowed tokens for function symbols and variables.
-}
module TRSConversion.Parse.COPS.Term (
  -- * COPS Terms
  SymbolType(..),
  varsIn,
  funsIn,
  parseTerm,
  parseTermFuns,
  parseTermVars,
)
where

import TRSConversion.Parse.COPS.Utils (Parser, ident, parens, symbol)
import TRSConversion.Problem.Common.Term (Term (..))
import Text.Megaparsec (option, sepBy)

parseTermVars :: [String] -> Parser (Term String String)
parseTermVars vars = parseTerm (varsIn vars)

parseTermFuns :: [String] -> Parser (Term String String)
parseTermFuns funs = parseTerm (funsIn funs)

data SymbolType = FunSymb | VarSymb
  deriving (Eq, Ord, Show)

varsIn :: [String] -> String -> SymbolType
varsIn vs symb
  | symb `elem` vs = VarSymb
  | otherwise = FunSymb

funsIn :: [String] -> String -> SymbolType
funsIn fs symb
  | symb `elem` fs = FunSymb
  | otherwise = VarSymb

parseTerm :: (String -> SymbolType) -> Parser (Term String String)
parseTerm symbTest = go
 where
  go = do
    name <- ident
    case symbTest name of
      VarSymb -> pure $ Var name
      FunSymb -> Fun name <$> option [] (parens (sepBy go (symbol ",")))


