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

import TRSConversion.Parse.ARI.Utils (ARIParser, ident, keyword, naturalNumber, parens, sExpr)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import Text.Megaparsec (many, option)
import TRSConversion.Parse.ARI.Trs (parseSystems)

parseAriCSTrs :: ARIParser (CSTrs String String)
parseAriCSTrs = do
  n <- pFormat
  (sig, repMap) <- pSignatureReplacementMap
  rs <- parseSystems sig
  return $
    CSTrs
      { rules = rs
      , signature = FunSig sig
      , replacementMap = repMap
      , numSystems = n
      }

pFormat :: ARIParser Int
pFormat = sExpr "format" $ do
  _ <- keyword "CSTRS"
  option 1 (keyword ":number" >> naturalNumber)

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

