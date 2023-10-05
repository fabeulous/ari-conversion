{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.ARI.Infeasibility where

import Control.Applicative (many)
import TRSConversion.Parse.ARI.CTrs (parseAriCTrs', parseCondition, pCondType)
import TRSConversion.Parse.ARI.Utils (ARIParser, sExpr, keyword)
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), CondType, Condition, inferSigFromRules)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Problem.Trs.TrsSig (TrsSig(FunSig))

parseAriInfeasibility :: ARIParser (Infeasibility String String)
parseAriInfeasibility = do
  condType <- sExpr "format" $ keyword "CTRS" *> pCondType <* keyword ":problem" <* keyword "infeasibility"
  parseAriInfeasibility' condType


parseAriInfeasibility' :: CondType -> ARIParser (Infeasibility String String)
parseAriInfeasibility' condType = do
    sys <- parseAriCTrs' condType 1
    let FunSig funSig = signature sys
    q <- parseInfQuery funSig
    pure $ Infeasibility{ctrs = sys, query = q}

parseInfQuery :: [Sig String] -> ARIParser [Condition String String]
parseInfQuery funSig =
    sExpr "infeasible?" $ many (parseCondition funSig)
