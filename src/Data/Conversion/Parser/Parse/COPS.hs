{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Parser.Parse.COPS
  ( parseCOPS,
  )
where

import Data.Conversion.Parser.Parse.Problem.Rule (parseRules)
import Data.Conversion.Parser.Parse.Problem.Sig (parseSig)
import Data.Conversion.Parser.Parse.Problem.Term (parseVariable)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parens, stripSpaces)
import Data.Conversion.Problem.Rule (Rule (..))
import Data.Conversion.Problem.Term (Term (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Text (pack)
import Text.Megaparsec
  ( many,
    noneOf,
    optional,
    parseTest,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (ErrorFancy (..))

testRules :: [Rule String String]
testRules = [r1, r2]

r1 = Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}

r2 = Rule {lhs = Fun "g" [Var "x", Fun "f" [Fun "f" [Var "y"]]], rhs = Fun "f" [Var "z"]}

-- | Parse a problem in COPS format
-- qqjf copy in COPS grammar and add detailed documentation, especially about sig
-- Should deduce
parseCOPS :: Parser (Trs String String)
parseCOPS = stripSpaces $ do
  vs <- try (block "VAR" (many parseVariable)) <|> return []
  inputSig <- optional (try $ block "SIG" parseSig)
  (rs, sig) <- block "RULES" (parseRules vs inputSig)
  comment <- optional (block "COMMENT" parseComment)
  return
    ( Trs
        { rules = rs,
          variables = vs,
          signature = sig,
          comment = comment
        }
    )
  where
    block :: String -> Parser a -> Parser a
    block name p =
      parens
        ( lexeme (string $ pack name) -- Parse block name
            *> lexeme p
        )
        <?> (name ++ " block")

-- | Parser to extract comments as a @String@ from a @COMMENT@ block of the COPS TRS format
-- Runs recursively to allow for parentheses inside comments qqjf
-- Copied from library term-rewriting
parseComment :: Parser String
parseComment =
  withParens
    <|> (++) <$> some (noneOf ['(', ')']) <*> parseComment
    <|> return ""
  where
    withParens :: Parser String
    withParens = do
      _ <- char '('
      pre <- parseComment
      _ <- char ')'
      suf <- parseComment
      return $ "(" ++ pre ++ ")" ++ suf

t0 = parseTest parseCOPS "(VAR x y)(SIG (f 2) (a 0) (b 1))(RULES f(x,y)->y)" -- (COMMENT a test comment (with parens))" -- "

t1 = parseTest parseCOPS "(VAR x y) (RULES f(x)->y \n f(x,y)->y )" -- (COMMENT a test comment (with parens))" -- "

t2 = parseTest parseCOPS "(VAR x y) (RULES f(x,y)->f(x) \n f(x,y)->y )" -- (COMMENT a test comment (with parens))" -- "
--
