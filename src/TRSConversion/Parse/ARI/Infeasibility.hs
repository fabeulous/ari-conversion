{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.ARI.Infeasibility where

import Control.Applicative (many)
import TRSConversion.Parse.ARI.CTrs (parseAriCTrs', parseCondition)
import TRSConversion.Parse.ARI.Utils (ARIParser, sExpr)
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), CondType, Condition, inferSigFromRules)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import TRSConversion.Problem.Trs.Sig (Sig)
import qualified TRSConversion.Problem.Trs.TrsSig as Sig

parseAriInfeasibility' :: CondType -> ARIParser (Infeasibility String String)
parseAriInfeasibility' condType = do
    sys <- parseAriCTrs' condType 1
    let funSig = case signature sys of
            (Sig.Vars _) -> case inferSigFromRules (concat (rules sys)) of
                Left e -> fail e
                Right sig -> sig
            (Sig.FunSig fs) -> fs
            (Sig.FullSig _ fs) -> fs
    q <- parseInfQuery funSig
    pure $ Infeasibility{ctrs = sys, query = q}

parseInfQuery :: [Sig String] -> ARIParser [Condition String String]
parseInfQuery funSig =
    sExpr "infeasible?" $ many (parseCondition funSig)
