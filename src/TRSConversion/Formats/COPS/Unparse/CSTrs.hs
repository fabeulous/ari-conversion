{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Formats.COPS.Unparse.CSTrs
Description : Unparser for CSTRSs
-}
module TRSConversion.Formats.COPS.Unparse.CSTrs (
  -- * COPS
  unparseCopsCSTrs,
  -- ** Helpers
  copsReplacementMap,
  unparseCopsCsTRSRules
)
where

import qualified Data.IntMap as IntMap
import Data.List (group, sort)
import Prettyprinter (Doc, Pretty, comma, hardline, hsep, nest, parens, pretty, punctuate, space, vsep)

import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import qualified TRSConversion.Problem.Common.Rule as R
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Formats.COPS.Unparse.Problem.Rule (unparseCopsRule)
import TRSConversion.Unparse.Utils (prettyBlock)

{- | Unparse a CSTRS from the Haskell 'CSTrs' representation into
[COPS CSTRS format](http://project-coco.uibk.ac.at/problems/cstrs.php).
-}
unparseCopsCSTrs :: (Ord v, Pretty f, Pretty v) => CSTrs f v -> Either String (Doc ann)
unparseCopsCSTrs cstrs
  | numSystems cstrs /= 1 = error "COPS format doesn't support CSTRSs with multiple systems"
  | otherwise = do
      pure $
        vsep
          [ prettyBlock "VAR" (hsep [pretty v | v <- vs])
          , prettyBlock "REPLACEMENT-MAP" (nest 2 (hardline <> copsReplacementMap (replacementMap cstrs)) <> hardline)
          , prettyBlock "RULES" (nest 2 (vsep $ mempty : [unparseCopsRule r | r <- rs]) <> hardline)
          ]
 where
  rs = rules cstrs IntMap.! 1
  vs = varsOfTrs rs

  varsOfTrs rls = map head . group . sort $ concatMap varsOfRules rls

  varsOfRules r = vars (R.lhs r) ++ vars (R.rhs r)

unparseCopsCsTRSRules :: (Pretty f, Pretty v) => [R.Rule f v] -> Doc ann
unparseCopsCsTRSRules rs =
  prettyBlock "RULES" (nest 2 (vsep $ mempty : [unparseCopsRule r | r <- rs]) <> hardline)

copsReplacementMap :: Pretty f => ReplacementMap f -> Doc ann
copsReplacementMap repMap =
  vsep
    [ parens $
      pretty f
        <> if null poss
          then mempty
          else space <> hsep (punctuate comma (map pretty poss))
    | (f, poss) <- repMap
    ]

