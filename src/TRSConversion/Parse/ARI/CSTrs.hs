{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Data.Conversion.Parse.ARI.CSTrs
Description : Parser for CSTRSs in ARI format
-}
module TRSConversion.Parse.ARI.CSTrs (
  parseAriCSTrs,
)
where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Void (Void)
import TRSConversion.Parse.ARI.Rule (parseAriRule)
import TRSConversion.Parse.ARI.Utils (ident, keyword, naturalNumber, parens, sExpr)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import Text.Megaparsec (Parsec, many, option)

type Parser = Parsec Void Text

parseAriCSTrs :: Parser (CSTrs String String)
parseAriCSTrs = do
  _ <- sExpr "format" (keyword "CSTRS")
  (sig, repMap) <- pSignatureReplacementMap
  rs <- pRules sig
  return $
    CSTrs
      { rules = rs
      , signature = FunSig sig
      , replacementMap = repMap
      }

pSignatureReplacementMap :: Parser ([Sig String], ReplacementMap String)
pSignatureReplacementMap =
  foldl' (\(sigs, reps) (sig, rep) -> (sig : sigs, rep ++ reps)) ([], []) <$> many (sExpr "fun" pSigRep)

pSigRep :: Parser (Sig String, ReplacementMap String)
pSigRep = do
  funSymb <- ident
  arity <- naturalNumber
  repMap <- option [] ((: []) <$> pReplacementMap funSymb)
  pure (Sig funSymb arity, repMap)
 where

  pReplacementMap :: String -> Parser (String, [Int])
  pReplacementMap f = keyword ":replacement-map" *>  ((f,) <$> parens (many naturalNumber))

pRules :: [Sig String] -> Parser [Rule String String]
pRules funSig = many (sExpr "rule" (parseAriRule funSig))
