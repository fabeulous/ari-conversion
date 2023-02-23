{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Problem.Rule.Parse
  ( parseRule,
  )
where

import Data.Conversion.Problem.Rule.Type (Rule (..))
import Data.Conversion.Problem.Term.Parse (parseTerm)
import Data.Conversion.Problem.Term.Type (Term)
import Data.Conversion.Utils (Parser, lexeme)
import Text.Megaparsec
  ( (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (string)

type Vars = [String]

-- | Parse a single rule separated by "->"
-- qqjf what if multiple -> s?
parseRule :: Vars -> Parser (Rule String String)
parseRule vs = do
  l <- parseTerm vs <?> "left-hand side"
  _ <- lexeme (string "->")
  r <- parseTerm vs <?> "right-hand side"
  return (Rule {lhs = l, rhs = r})
