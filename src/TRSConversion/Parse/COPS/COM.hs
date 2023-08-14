{-# LANGUAGE OverloadedStrings #-}

-- | parse for the commutation problems in COPS
module TRSConversion.Parse.COPS.COM (parseCopsCom) where

import qualified Data.IntMap as IntMap
import Data.Text (unpack)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, keyword)
import TRSConversion.Problem.Trs.Trs (Trs (..), TrsSig (..))
import Text.Megaparsec (MonadParsec (takeWhileP), option)
import TRSConversion.Parse.COPS.Trs (parseCopsTrs)
import Data.Containers.ListUtils (nubOrd)
import TRSConversion.Problem.Common.Rule (inferSigFromRules)
import Data.Foldable (toList)
import Data.Either (fromRight)

parseCopsCom :: COPSParser (Maybe String, Trs String String)
parseCopsCom = do
    _ <- block "PROBLEM" (keyword "COMMUTATION")
    origin <- option Nothing (fmap (Just . unpack) $ block "COMMENT" $ takeWhileP (Just "comment") (/= ')'))
    trs1 <- parseCopsTrs
    trs2 <- parseCopsTrs
    pure
        ( origin
        , mergeTrss trs1 trs2
        )

mergeTrss :: (Ord v, Ord f) => Trs f v -> Trs f v -> Trs f v
mergeTrss trs1 trs2 =
  Trs { rules = IntMap.union (rules trs1) rules2'
      , signature = mergeSigs (signature trs1) (signature trs2)
      , numSystems = n1 + n2
      }
  where
    n1 = numSystems trs1
    n2 = numSystems trs2
    rules2' = IntMap.mapKeysMonotonic (n1+) (rules trs2)
    mergeSigs sig1 sig2 =
        case (sig1, sig2) of
            (Vars vs1, Vars vs2) -> Vars (nubOrd $ vs1 ++ vs2)
            (FunSig fs1, FunSig fs2) -> FunSig (nubOrd $ fs1 ++ fs2)
            (FullSig vs1 fs1, FullSig vs2 fs2) -> FullSig (nubOrd $ vs1 ++ vs2) (nubOrd $ fs1 ++ fs2)
            (_, _) ->
              let f1 = fromRight (error "signatures cannot be merged") $ inferSigFromRules (concat (toList (rules trs1)))
                  f2 = fromRight (error "signatures cannot be merged") $ inferSigFromRules (concat (toList (rules trs2)))
               in  mergeSigs (FunSig f1) (FunSig f2)
