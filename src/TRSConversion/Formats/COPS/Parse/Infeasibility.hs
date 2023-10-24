{-# LANGUAGE OverloadedStrings #-}
module TRSConversion.Formats.COPS.Parse.Infeasibility (
    parseCopsInfeasibility,
)
where

import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (unpack)
import Text.Megaparsec (
    MonadParsec (takeWhileP, try),
    option,
    sepBy1,
 )

import TRSConversion.Formats.COPS.Parse.CTrs (pCRulesBlock, pCondTypeBlock, parseCopsCondition)
import TRSConversion.Formats.COPS.Parse.Trs (parseCopsVarBlock)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, block, keyword, symbol)
import TRSConversion.Problem.CTrs.CTrs (CRule (CRule), CTrs (..), CondType (Oriented), Condition ((:==)), inferSigFromRules)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig (FunSig))

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
