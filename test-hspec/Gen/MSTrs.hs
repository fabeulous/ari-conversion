{-# LANGUAGE TupleSections #-}

module Gen.MSTrs where

import Control.Monad (replicateM)
import qualified Data.Map as M
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..), inferSorts)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs (..), Rule (Rule), Term (..))
import qualified TRSConversion.Problem.MsTrs.MsTrs as MsTrs
import qualified Data.IntMap as IntMap

genMsRule :: (Ord s) => [MsSig f s] -> Gen v -> Gen (Rule f (v, s))
genMsRule sig varGen = do
  s <- Gen.element srts
  l <- genTermOfSort s
  r <- genTermOfSort s
  pure $ Rule l r
 where
  srts = inferSorts sig

  getFSigOfSort = \sort -> M.findWithDefault [] sort mp
   where
     mp = M.fromListWith (++) [(s, [(f, ss)]) | MsSig f (ss, s) <- sig]

  getConstOfSort = \sort -> M.findWithDefault [] sort mp
   where
    mp = M.fromListWith (++) [(s, [f]) | MsSig f ([], s) <- sig]

  genTermOfSort s =
    Gen.recursive
      Gen.choice
      ((Var . (,s) <$> varGen) : [pure $ Fun f [] | f <- getConstOfSort s])
      ( (Var . (,s) <$> varGen)
          : [ Fun f <$> traverse genTermOfSort ss
            | (f, ss) <- getFSigOfSort s
            ]
      )

genMsTrs :: Ord s => [MsSig f s] -> Gen v -> Gen (MsTrs f (v, s) s)
genMsTrs sig varGen = do
  nRules <- Gen.integral (Range.constantFrom 1 3 5)
  rs <- replicateM nRules (genMsRule sig varGen)
  pure $
    MsTrs
      { sorts = Just $ inferSorts sig
      , rules = IntMap.singleton 1 rs
      , signature = sig
      , numSystems = 1
      }

genMsTrsString :: [MsSig String String] -> Gen String -> Gen (MsTrs String String String)
genMsTrsString sig varGen = MsTrs.map id toStr id <$> genMsTrs sig varGen
  where
    toStr (v,s) = v ++ "_" ++ s
