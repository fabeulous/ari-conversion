{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Formats.ARI.Parse.CSTrs
Description : Parser for CSTRSs in ARI format
-}
module TRSConversion.Formats.ARI.Parse.CSTrs (
  parseAriCSTrs,
  parseAriCSTrs',
  -- * Parsers
  pSignatureReplacementMap
)
where

import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, ident, keyword, naturalNumber, parens, sExpr, index, nonPositiveNumberError, indexOutOfRangeError, duplicateIndex, FunSymb, VarSymb)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import Text.Megaparsec (many, option, MonadParsec (parseError))
import TRSConversion.Formats.ARI.Parse.Trs (parseSystems)
import qualified TRSConversion.Problem.Common.Index as Idx
import Control.Monad
import qualified Data.Set as Set
import TRSConversion.Parse.Utils (Token)

parseAriCSTrs :: ARIParser (CSTrs FunSymb VarSymb)
parseAriCSTrs = pFormat >>= parseAriCSTrs'

parseAriCSTrs' :: Int -> ARIParser (CSTrs FunSymb VarSymb)
parseAriCSTrs' n = do
  (sig, repMap) <- pSignatureReplacementMap
  rs <- parseSystems n sig
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

pSignatureReplacementMap :: ARIParser ([Sig FunSymb], ReplacementMap FunSymb)
pSignatureReplacementMap =
  foldr (\(sig, rep) (sigs, reps) -> (sig : sigs, rep ++ reps)) ([], []) <$> many (sExpr "fun" pSigRep)

pSigRep :: ARIParser (Sig FunSymb, ReplacementMap FunSymb)
pSigRep = do
  funSymb <- ident
  arity <- naturalNumber
  repMap <- option [] ((: []) <$> pReplacementMap funSymb arity)
  pure (Sig funSymb arity, repMap)
 where

  pReplacementMap :: Token String -> Int -> ARIParser (FunSymb, [Int])
  pReplacementMap f arity = do
    _ <- keyword ":replacement-map"
    indices <- parens (many index)
    forM_ indices $ \idx@(Idx.Index n offSet) -> do
      unless (n > 0) $ parseError (nonPositiveNumberError n offSet)
      unless (n <= arity) $ parseError (indexOutOfRangeError arity idx)
    foldM_ (\set idx ->
                  if Idx.index idx `Set.member` set
                  then parseError (duplicateIndex idx) >> pure set
                  else pure $ Set.insert (Idx.index idx) set
               ) Set.empty indices
    pure (f,map Idx.index indices)
