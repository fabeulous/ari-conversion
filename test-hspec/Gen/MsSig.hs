
module Gen.MsSig where

import Control.Monad (replicateM)
import Data.Traversable (forM)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))

genMsSig :: Gen [MsSig String String]
genMsSig = do
    sigSize <- Gen.integral (Range.constantFrom 1 10 25)
    let fs = ["f" <> show i | i <- [1 .. sigSize :: Int]]
    nSorts <- Gen.integral (Range.constantFrom 1 1 sigSize)
    let genS = Gen.element ["s" <> show i | i <- [1 .. nSorts :: Int]]
    forM fs $ \f -> do
        arity <- Gen.integral (Range.constantFrom 0 0 5)
        args <- replicateM arity genS
        res <- genS
        pure $ MsSig f (args, res)
