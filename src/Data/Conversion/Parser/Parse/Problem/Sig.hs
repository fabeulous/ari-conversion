-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.Sig
-- Description : Signature parser
--
-- This module defines 'parseSig' to parse a TRS signature from a @String@ input.
module Data.Conversion.Parser.Parse.Problem.Sig
  ( parseSig,
  )
where

import Data.Conversion.Parser.Parse.Problem.Term (parseFunSymbol)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parens, stripSpaces)
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Text.Megaparsec (many, some)
import Text.Megaparsec.Char (digitChar)

-- | Parser to extract the signature from a @SIG@ block of the COPS [extended TRS format](http://project-coco.uibk.ac.at/problems/trs.php#extended).
-- Expects a sequence of blocks @(fsym Int)@ where the @Int@ is the arity of the given symbol (see examples below).
-- Leading and trailing spaces are ignored.
--
-- >>> parse parseSig "" (pack "(f 2) (a 0) (h 1)")
-- Right [Sig "f" 2,Sig "a" 0,Sig "h" 1]
parseSig :: Parser [Sig String]
parseSig = stripSpaces $ many fundecl
  where
    fundecl =
      lexeme $
        parens
          ( do
              fsym <- parseFunSymbol
              arity <- lexeme (read <$> some digitChar)
              return $ Sig fsym arity
          )
