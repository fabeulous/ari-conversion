{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.COPS.Infeasibility where

import qualified Data.IntMap as IntMap
import Data.Text (unpack)
import TRSConversion.Parse.COPS.CTrs (pCRulesBlock, pCondTypeBlock, parseCopsCondition)
import TRSConversion.Parse.COPS.Trs (parseCopsVarBlock)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, keyword, symbol)
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), CondType (Oriented), inferSigFromRules, Condition ((:==)), CRule (CRule))
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import Text.Megaparsec (
    MonadParsec (takeWhileP, try),
    option,
    sepBy1,
 )
import TRSConversion.Problem.Trs.TrsSig (TrsSig(FunSig))
import Data.Maybe (fromMaybe, isNothing)

parseCopsInfeasibility :: COPSParser (Maybe String, Infeasibility String String)
parseCopsInfeasibility = do
    _ <- try $ block "PROBLEM" (keyword "INFEASIBILITY")
    origin <-
        option Nothing (fmap (Just . unpack) $ block "COMMENT" $ takeWhileP (Just "comment") (/= ')'))
    -- system <- parseCopsCTrs
    condType <- option Nothing (Just <$> pCondTypeBlock)
    vars <- parseCopsVarBlock
    rs <- pCRulesBlock vars
    vars2 <- parseCopsVarBlock
    q <- block "CONDITION" $ sepBy1 (parseCopsCondition (vars ++ vars2)) (symbol ",")
    -- small hack: turn query conditions into rules to infer signature
    sig <- case inferSigFromRules (rs ++ [CRule l r [] | l :== r <- q]) of
      Left err -> fail err
      Right fs -> pure $ FunSig fs

    let system =
            CTrs
                { conditionType = fromMaybe Oriented condType
                , rules = IntMap.singleton 1 rs
                , signature = sig
                , numSystems = 1
                }
    return
        ( origin
        , Infeasibility
            { ctrs = system
            , query = q
            , isTrs = isNothing condType
            }
        )
