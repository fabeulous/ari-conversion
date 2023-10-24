{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.COPS.CSCTrs
Description : Unparser for CSCTRSs
-}
module TRSConversion.Unparse.COPS.CSCTrs (
  -- * COPS
  unparseCopsCSCTrs,
)
where

import Data.List (group, sort)
import Prettyprinter (Doc, Pretty, hardline, hsep, nest, parens, pretty, vsep, (<+>))
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), inferSigFromRules)
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..), Sig (..))
import TRSConversion.Unparse.COPS.CSTrs (copsReplacementMap)
import TRSConversion.Unparse.COPS.CTrs (prettyCRule, prettyCondType)
import TRSConversion.Unparse.COPS.Utils (filterEmptyDocs, prettyBlock)
import TRSConversion.Problem.CSTrs.CSTrs (ReplacementMap)
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IntMap

{- | Unparse a first-order TRS from the Haskell 'Trs' representation into
[COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).

Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
unparse each part of the 'Trs'.

See the tests for examples of expected output.
-}
unparseCopsCSCTrs :: (Ord v, Pretty f, Pretty v) => CSCTrs f v -> Either String (Doc ann)
unparseCopsCSCTrs CSCTrs{ctrs = system, replacementMap = repMap}
  | numSystems system /= 1 = error "COPS format doesn't support CSCTRSs with multiple systems"
  | otherwise = do
      pure $
        vsep
          [ prettyBlock "CONDITIONTYPE" (prettyCondType $ conditionType system)
          , prettyBlock "VAR" (hsep [pretty v | v <- vs])
          , prettyBlock "REPLACEMENT-MAP" (nest 2 (hardline <> copsReplacementMap repMap) <> hardline)
          , prettyBlock "RULES" (nest 2 (vsep $ mempty : [prettyCRule r | r <- rs]) <> hardline)
          ]
 where
  rs = rules system IntMap.! 1
  vs = varsOfTrs rs

  varsOfTrs rls = map head . group . sort $ concatMap varsOfRules rls

  varsOfRules r = vars (lhs r) ++ vars (rhs r)

