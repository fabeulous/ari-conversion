module Data.Conversion.Parser.Parse.Problem.Sig
  ( parseSig,
  )
where

import Data.Conversion.Parser.Parse.Problem.Term (parseFunSymbol)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parens)
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Text.Megaparsec (many, some)
import Text.Megaparsec.Char (digitChar)

-- | Parser to extract the signature from a SIG block of the COPS TRS format
--   For example, from a block @(SIG (f 2) (a 0) (b 1))@ we want to extract a list @[("f",2),("a",0),("b",1)]@
parseSig :: Parser [Sig String]
parseSig = many fundecl
  where
    fundecl =
      parens
        ( do
            fsym <- parseFunSymbol
            arity <- lexeme (read <$> some digitChar)
            return $ Sig fsym arity
        )
