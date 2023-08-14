{-# LANGUAGE OverloadedStrings #-}

-- | parse for the commutation problems in COPS
module TRSConversion.Parse.COPS.COM (parseCopsCom) where

import Control.Applicative (many)
import qualified Data.IntMap as IntMap
import Data.Text (unpack)
import TRSConversion.Parse.COPS.Rule (parseCopsTrsRules)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, ident, keyword)
import TRSConversion.Problem.Trs.Trs (Trs (..), TrsSig (..))
import Text.Megaparsec (MonadParsec (takeWhileP), option)

parseCopsCom :: COPSParser (Maybe String, Trs String String)
parseCopsCom = do
    _ <- block "PROBLEM" (keyword "COMMUTATION")
    origin <- option Nothing (fmap (Just . unpack) $ block "COMMENT" $ takeWhileP (Just "comment") (/= ')'))
    vs1 <- block "VAR" (many ident)
    rules1 <- block "RULES" (parseCopsTrsRules (Vars vs1))
    vs2 <- block "VAR" (many ident)
    let vs = vs1 ++ vs2
    rules2 <- block "RULES" (parseCopsTrsRules (Vars vs))
    pure
        ( origin
        , Trs
            { rules = IntMap.fromList [(1, rules1), (2, rules2)]
            , signature = Vars vs
            , numSystems = 2
            }
        )
