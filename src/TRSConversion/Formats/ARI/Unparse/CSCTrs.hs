{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Formats.ARI.Unparse.CSCTrs
Description : Unparser for CSCTRSs
-}
module TRSConversion.Formats.ARI.Unparse.CSCTrs (
  -- * ARI
  unparseAriCSCTrs,
)
where

import qualified Data.Map.Strict as M
import Prettyprinter (Doc, Pretty, hsep, parens, pretty, vsep, (<+>))

import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CSTrs.CSTrs (ReplacementMap)
import TRSConversion.Problem.CTrs.CTrs (CTrs (..))
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import TRSConversion.Formats.ARI.Unparse.CTrs (prettyAriConditionType, unparseAriCSystems)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

unparseAriCSCTrs :: (Pretty f, Pretty v, Ord f) => CSCTrs f v -> Either String (Doc ann)
unparseAriCSCTrs CSCTrs{ctrs = system, replacementMap = repMap} = do
  ariSig <- unparseAriCSCTrsSig (signature system) repMap
  let trsElements =
        [ parens $ "format CSCTRS" <+> prettyAriConditionType (conditionType system)
        , ariSig
        , unparseAriCSystems (rules system)
        ]
  return $ vsep (filterEmptyDocs trsElements)

unparseAriCSCTrsSig :: (Ord f, Pretty f) => TrsSig f -> ReplacementMap f -> Either String (Doc ann)
unparseAriCSCTrsSig (FunSig fs) repMap = Right (vsep $ map prettySigLine fs)
 where
  repMapM = M.fromList repMap
  prettyM = M.map (\ints -> mempty <+> ":replacement-map" <+> (parens . hsep $ map pretty ints)) repMapM

  prettySigLine (Sig f a) = parens $ "fun" <+> pretty f <+> pretty a <> M.findWithDefault mempty f prettyM

