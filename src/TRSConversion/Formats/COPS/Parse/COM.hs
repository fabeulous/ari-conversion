{-# LANGUAGE OverloadedStrings #-}

-- | parse for the commutation problems in COPS
module TRSConversion.Formats.COPS.Parse.COM (parseCopsCom) where

import qualified Data.IntMap as IntMap
import Data.Text (unpack)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, block, keyword)
import TRSConversion.Problem.Trs.Trs (Trs (..), TrsSig (..))
import Text.Megaparsec (MonadParsec (takeWhileP), option, try)
import TRSConversion.Formats.COPS.Parse.Trs (parseCopsTrs)
import Data.Containers.ListUtils (nubOrd)

parseCopsCom :: COPSParser (Maybe String, Trs String String)
parseCopsCom = do
    _ <- try $ block "PROBLEM" (keyword "COMMUTATION")
    origin <- option Nothing (fmap (Just . unpack) $ block "COMMENT" $ takeWhileP (Just "comment") (/= ')'))
    trs1 <- parseCopsTrs
    trs2 <- parseCopsTrs
    pure
        ( origin
        , mergeTrss trs1 trs2
        )

mergeTrss :: (Ord f) =>Trs f v -> Trs f v -> Trs f v
mergeTrss trs1 trs2 =
  Trs { rules = IntMap.union (rules trs1) rules2'
      , signature = mergeSigs (signature trs1) (signature trs2)
      , numSystems = n1 + n2
      }
  where
    n1 = numSystems trs1
    n2 = numSystems trs2
    rules2' = IntMap.mapKeysMonotonic (n1+) (rules trs2)
    mergeSigs (FunSig fs1) (FunSig fs2) = FunSig (nubOrd $ fs1 ++ fs2)
