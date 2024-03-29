{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.Term
Description : Term, function symbol, and variable parser

This module defines functions to parse terms from a @String@ input.
Also specifies the allowed tokens for function symbols and variables.
-}
module TRSConversion.Formats.ARI.Parse.Term (
    -- * ARI Terms
    parsePrefixTerm,

    -- * Functions and Variables
)
where

-- parseFunSymbol,
-- parseVariable,

import qualified Data.Map.Strict as M
import Text.Megaparsec (count, getOffset, parseError, (<|>), hidden)
import qualified Text.Megaparsec.Error.Builder as E

import TRSConversion.Formats.ARI.Parse.Utils (
    ARIParser,
    FunSymb,
    VarSymb,
    parens,
    restrictedIdent,
 )
import TRSConversion.Parse.Utils (Token (tokenText))
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))

{- | Parses a term in prefix notation (see also [S-expressions](https://en.wikipedia.org/wiki/S-expression)).

Tries to parse the expression as a function application first, then as a constant, and finally as a variable.
This order of priority is important as 'parsePrefixTerm' does not consume the entire input.
For example, @f x@ should be parsed as @Fun "f" [Var "x"]@, rather than as a constant @f@ with trailing input @x@.

Consumes trailing white space only after the term has been recursively parsed as far as possible.

>>> parseTest (parseTerm [Sig "f" 2, Sig "g" 1]) "f x (g c)"
Fun "f" [Var "x", Fun "g" [Fun "x" []]]
-}
parsePrefixTerm :: [Sig FunSymb] -> ARIParser (Term FunSymb VarSymb)
parsePrefixTerm funSig = parseTerm
  where
    mp = M.fromList [(f, arity) | Sig f arity <- funSig]

    parseTerm = fun <|> constantOrVar
    -- <|> parens parseT -- how about redundant parenthesis?

    constantOrVar = do
        o <- getOffset
        name <- restrictedIdent <|> parseError (E.err o $ E.elabel "constant" <> E.elabel "variable")
        case M.lookup name mp of
            Nothing -> pure $ Var name
            Just a
                | a == 0 -> pure $ Fun name []
                | otherwise ->
                    parseError
                        $ E.err o
                        $ E.utoks (tokenText name)
                        <> E.elabel "constant"
                        <> E.elabel "variable"

    fun = parens $ do
        o <- getOffset
        name <- restrictedIdent
        case M.lookup name mp of
            Just arity | arity > 0 -> Fun name <$> count arity parseTerm
            _ ->
                parseError
                    $ E.err o
                    $ E.utoks (tokenText name)
                    <> E.elabel "non-constant function symbol"
