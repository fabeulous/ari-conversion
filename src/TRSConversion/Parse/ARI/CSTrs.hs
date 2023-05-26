{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Data.Conversion.Parse.ARI.CSTrs
Description : Parser for CSTRSs in ARI format
-}
module TRSConversion.Parse.ARI.CSTrs (
  parseAriCSTrs,
  -- * Parsers
  pSignatureReplacementMap
)
where

import TRSConversion.Parse.ARI.Rule (parseAriRule)
import TRSConversion.Parse.ARI.Utils (ARIParser, ident, keyword, naturalNumber, parens, sExpr)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import Text.Megaparsec (many, option)

parseAriCSTrs :: ARIParser (CSTrs String String)
parseAriCSTrs = do
  _ <- sExpr "format" (keyword "CSCTRS")
  (sig, repMap) <- pSignatureReplacementMap
  rs <- pRules sig
  return $
    CSTrs
      { rules = rs
      , signature = FunSig sig
      , replacementMap = repMap
      }

pSignatureReplacementMap :: ARIParser ([Sig String], ReplacementMap String)
pSignatureReplacementMap =
  foldr (\(sig, rep) (sigs, reps) -> (sig : sigs, rep ++ reps)) ([], []) <$> many (sExpr "fun" pSigRep)

pSigRep :: ARIParser (Sig String, ReplacementMap String)
pSigRep = do
  funSymb <- ident
  arity <- naturalNumber
  repMap <- option [] ((: []) <$> pReplacementMap funSymb)
  pure (Sig funSymb arity, repMap)
 where

  pReplacementMap :: String -> ARIParser (String, [Int])
  pReplacementMap f = keyword ":replacement-map" *>  ((f,) <$> parens (many naturalNumber))

pRules :: [Sig String] -> ARIParser [Rule String String]
pRules funSig = many (parseAriRule funSig)
