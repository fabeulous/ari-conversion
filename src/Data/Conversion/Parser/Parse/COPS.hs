{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Parser.Parse.Cops
  ( parseCops,
  )
where

import Data.Conversion.Parser.Parse.Problem.Rule (parseRules)
import Data.Conversion.Parser.Parse.Problem.Sig (parseSig)
import Data.Conversion.Parser.Parse.Problem.Term (parseVariable)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parens, stripSpaces)  
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Text (pack)
import Text.Megaparsec
  ( many,
    noneOf,
    optional,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, string)

-- | Parse a problem in COPS format
-- qqjf copy in COPS grammar and add detailed documentation, especially about sig
-- Should deduce
parseCops :: Parser (Trs String String)
parseCops = stripSpaces $ do
  vs <- try (block "VAR" (many parseVariable)) <|> return []
  inputSig <- optional (try $ block "SIG" parseSig)
  (rs, sig) <- block "RULES" (parseRules vs inputSig)
  metaInfo <- optional (block "COMMENT" parseComment)
  return
    ( Trs
        { rules = rs,
          variables = vs,
          signature = sig,
          comment = metaInfo
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