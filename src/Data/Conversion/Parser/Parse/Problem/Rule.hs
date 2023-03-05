{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Parser.Parse.Problem.Rule
  ( parseRule,
    parseRules,
  )
where

import Data.Conversion.Parser.Parse.Problem.Term (parseTerm)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme)
import Data.Conversion.Problem.Rule (Rule (..), ruleFunArities)
import Data.Conversion.Problem.Trs.Sig (Sig, checkConsistentSig)
import Data.List (nub)
import Text.Megaparsec (many, (<?>))
import Text.Megaparsec.Char (string)

type Vars = [String]

-- | Parse a single rule separated by "->"
-- qqjf what if multiple -> s?
parseRule :: Vars -> Parser (Rule String String)
parseRule vs = do
  l <- parseTerm vs <?> "left-hand side"
  _ <- lexeme (string "->")
  r <- parseTerm vs <?> "right-hand side"
  return $ Rule {lhs = l, rhs = r}

-- | Parser to extract the 'Rule's from a RULES block of the COPS TRS format
--   Expects format @lhs -> rhs@ and calls the 'Term' parser 'parseTerm' on each side of the rule
-- qqjf succeeds on no rules
-- If sig is Just Sig then require every function symbol to be here, otherwise deduce the signature
parseRules :: Vars -> Maybe [Sig String] -> Parser ([Rule String String], [Sig String])
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
    -- \| Infer signature from a list of rules
    inferSignature :: [Rule String String] -> Parser [Sig String]
    inferSignature rs = do
      case mapM ruleFunArities rs of
        Left err -> fail err
        Right sigLists -> case checkConsistentSig $ nub (concat sigLists) of
          Left err -> fail err
          Right sig -> return sig
    -- \| Return whether every element of the first list is contained in the second list
    subList :: Eq a => [a] -> [a] -> Bool
    subList xs ys = all (`elem` ys) xs