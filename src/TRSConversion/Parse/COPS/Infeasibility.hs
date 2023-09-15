{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.COPS.Infeasibility where

import qualified Data.IntMap as IntMap
import Data.Text (unpack)
import TRSConversion.Parse.COPS.CTrs (pCRulesBlock, pCondTypeBlock, parseCopsCondition)
import TRSConversion.Parse.COPS.Trs (parseCopsVarBlock)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, keyword, symbol)
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), CondType (Oriented))
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import qualified TRSConversion.Problem.Trs.TrsSig as Sig
import Text.Megaparsec (
    MonadParsec (takeWhileP, try),
    option,
    sepBy1,
 )
import Data.Containers.ListUtils (nubOrd)

parseCopsInfeasibility :: COPSParser (Maybe String, Infeasibility String String)
parseCopsInfeasibility = do
    _ <- try $ block "PROBLEM" (keyword "INFEASIBILITY")
    origin <-
        option Nothing (fmap (Just . unpack) $ block "COMMENT" $ takeWhileP (Just "comment") (/= ')'))
    -- system <- parseCopsCTrs
    condType <- option Oriented pCondTypeBlock
    vars <- parseCopsVarBlock
    rs <- pCRulesBlock vars
    vars2 <- parseCopsVarBlock
    q <- block "CONDITION" $ sepBy1 (parseCopsCondition (vars ++ vars2)) (symbol ",")
    let system =
            CTrs
                { conditionType = condType
                , rules = IntMap.singleton 1 rs
                , signature = Sig.Vars (nubOrd $ vars ++ vars2)
                , numSystems = 1
                }
    return
        ( origin
        , Infeasibility
            { ctrs = system
            , query = q
            }
        )
