{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.CSTrs
Description : Unparser for CSTRSs
-}
module TRSConversion.Unparse.CSTrs (
  -- * ARI
  unparseAriCSTrs,
  -- ** Helpers
  unparseAriReplacementSig,
)
where

import Data.List (group, sort)
import qualified Data.Map as M
import Prettyprinter (Doc, Pretty, comma, hardline, hsep, nest, parens, pretty, punctuate, space, vsep, (<+>))
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Common.Rule (inferSigFromRules)
import qualified TRSConversion.Problem.Common.Rule as R
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import TRSConversion.Unparse.Problem.Rule (unparseCopsRule, unparseAriSystems)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import qualified Data.IntMap as IntMap

unparseAriCSTrs :: (Pretty f, Pretty v, Ord f) => CSTrs f v -> Either String (Doc ann)
unparseAriCSTrs cstrs = do
  ariSig <- unparseAriReplacementSig (concat $ rules cstrs) (signature cstrs) (replacementMap cstrs)
  let trsElements =
        [ formatString
        , ariSig
        , unparseAriSystems (rules cstrs)
        ]
  return $ vsep (filterEmptyDocs trsElements)
 where
  n = numSystems cstrs
  formatString = parens $ "format" <+> "CSTRS" <>
    if n > 1 then mempty <+> ":number" <+> pretty n else mempty

unparseAriReplacementSig :: (Ord f, Pretty f) => [R.Rule f v] -> TrsSig f -> ReplacementMap f -> Either String (Doc ann)
unparseAriReplacementSig rs (FunSig fs) repMap = Right (vsep $ map prettySigLine fs)
 where
  repMapM = M.fromList repMap
  prettyM = M.map (\ints -> mempty <+> ":replacement-map" <+> (parens . hsep $ map pretty ints)) repMapM

  prettySigLine (Sig f a) = parens $ "fun" <+> pretty f <+> pretty a <> M.findWithDefault mempty f prettyM
