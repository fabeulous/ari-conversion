module TRSConversion.Problem.CTrs.CTrs (
    CTrs (..),
    CRule (..),
    Condition (..),
    CondType (..),
    inferSigFromRules,
) where

import qualified Data.Map.Strict as M
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig)
import Data.Foldable (foldl')

data CondType = Oriented | Join | SemiEquational
    deriving (Eq, Show)

data Condition f v = (Term f v) :== (Term f v)
    deriving (Eq, Show)

data CRule f v = CRule
    { lhs :: Term f v
    -- ^ The left-hand side of the rule
    , rhs :: Term f v
    -- ^ The right-hand side of the rule
    , conditions :: [Condition f v]
    -- ^ Conditions
    }
    deriving (Eq, Show)

data CTrs f v = CTrs
    { conditionType :: CondType
    , rules :: [CRule f v]
    -- ^ A list of the MSTRS rewrite rules
    , signature :: TrsSig f v
    -- ^ The signature (function symbols and corresponding sorts) for the MSTRS
    }
    deriving (Show, Eq)

inferSigFromRules :: Ord f => [CRule f v] -> Either String [Sig f]
inferSigFromRules ctrs = M.foldrWithKey (\f a acc -> Sig f a : acc) [] <$> resM
  where
    terms r = lhs r : rhs r : concatMap termsC (conditions r)
    termsC (t1 :== t2) = [t1, t2]

    overApproxSig = concatMap termFunArity $ concatMap terms ctrs

    resM =
        foldl'
            ( \mp (f, a) -> do
                mp' <- mp
                case mp' M.!? f of
                    Just b
                        | a == b -> mp
                        | otherwise -> Left "Error: function symbol with multiple arities found"
                    Nothing -> pure $ M.insert f a mp'
            )
            (Right M.empty)
            overApproxSig

termFunArity :: Term f v -> [(f, Int)]
termFunArity (Fun f ts) = (f, length ts) : concatMap termFunArity ts
termFunArity (Var _) = []
