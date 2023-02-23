{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Parser.Parse.COPS
  (
  )
where

import Control.Monad.Fail (fail)
import Data.Conversion.Problem.Rule (Rule (..), parseRule, ruleFunArities)
import qualified Data.Conversion.Problem.Rule as Rule
import Data.Conversion.Problem.TRS (TRS (..))
import qualified Data.Conversion.Problem.TRS as TRS
import Data.Conversion.Problem.Term (checkConsistentSig, foldTerm, parseFunSymbol, parseTerm, parseVariable)
import Data.Conversion.Utils (lex, lexeme, par, sc)
import Data.List (nub, sort)
import Data.Rewriting.Term.Type
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    between,
    eof,
    failure,
    many,
    noneOf,
    optional,
    parseTest,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    spaceChar,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ErrorFancy (..))
import Prelude hiding (lex)

type Parser = Parsec Void Text

type Vars = [String]
-- ^ Type for a list of variables

type Sig = [(String, Int)]
-- ^ Type for signature. Contains function symbol and arity. qqjf

-- | Parser to extract the 'Rule's from a RULES block of the COPS TRS format
--   Expects format @lhs -> rhs@ and calls the 'Term' parser 'parseTerm' on each side of the rule
-- qqjf succeeds on no rules
-- If sig is Just Sig then require every function symbol to be here, otherwise deduce the signature
parseRules :: Vars -> Maybe Sig -> Parser ([Rule String String], Sig)
parseRules vs maybeSig = do
  rules <- many (parseRule vs)
  inferredSig <- inferSignature rules
  case maybeSig of
    Just sig ->
      if inferredSig `subList` sig
        then return (rules, sig)
        else fail $ "Inferred signature " ++ show inferredSig ++ " not contained in input signature " ++ show sig
    Nothing -> return (rules, inferredSig)
  where
    checkSignature :: Sig -> Sig -> [Rule String String] -> Parser ([Rule String String], Sig)
    checkSignature sig inferredSig rs = undefined
    -- \| Infer signature from a list of rules
    inferSignature :: [Rule String String] -> Parser Sig
    inferSignature rs = do
      case mapM ruleFunArities rs of
        Left err -> fail err
        Right sigLists -> case checkConsistentSig $ nub (concat sigLists) of
          Left err -> fail err
          Right sig -> return sig
    -- \| Return whether every element of the first list is contained in the second list
    subList :: Eq a => [a] -> [a] -> Bool
    subList xs ys = all (`elem` ys) xs

testRules :: [Rule String String]
testRules = [r1, r2]

r1 = Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}

r2 = Rule {lhs = Fun "g" [Var "x", Fun "f" [Fun "f" [Var "y"]]], rhs = Fun "f" [Var "z"]}

funsDL :: Term f v -> [f] -> [f]
funsDL = foldTerm (const id) (\f xs -> (f :) . foldr (.) id xs)

-- | Strip spaces at start and end of a string
stripSpaces :: Parser a -> Parser a
stripSpaces p = lexeme (many spaceChar *> p)

-- | Parse a problem in COPS format
-- qqjf copy in COPS grammar and add detailed documentation, especially about sig
-- Should deduce
parseCOPS :: Parser (TRS String String)
parseCOPS = stripSpaces $ do
  vs <- try (block "VAR" (many parseVariable)) <|> return []
  inputSig <- optional (try $ block "SIG" parseSig)
  (rules, sig) <- block "RULES" (parseRules vs inputSig)
  comment <- optional (block "COMMENT" parseComment)
  return
    ( TRS
        { rules = rules,
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

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Strip outer parentheses
parens :: Parser a -> Parser a
parens = between (lexeme $ symbol "(") (lexeme $ symbol ")")

t0 = parseTest parseCOPS "(VAR x y)(SIG (f 2) (a 0) (b 1))(RULES f(x,y)->y)" -- (COMMENT a test comment (with parens))" -- "

t1 = parseTest parseCOPS "(VAR x y) (RULES f(x)->y \n f(x,y)->y )" -- (COMMENT a test comment (with parens))" -- "

t2 = parseTest parseCOPS "(VAR x y) (RULES f(x,y)->f(x) \n f(x,y)->y )" -- (COMMENT a test comment (with parens))" -- "
--

-- | Parser to extract the signature from a SIG block of the COPS TRS format
--   For example, from a block @(SIG (f 2) (a 0) (b 1))@ we want to extract a list @[("f",2),("a",0),("b",1)]@
parseSig :: Parser Sig
parseSig = many fundecl
  where
    fundecl =
      parens
        ( do
            fsym <- parseFunSymbol
            arity <- lexeme (read <$> some digitChar)
            return (fsym, arity)
        )
