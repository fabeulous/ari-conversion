module TRSConversion.Problem.CTrs.CTrs (
    CTrs (..),
    CRule (..),
    Condition (..),
    CondType (..),
    mapCondition,
    mapCRule,
    mapCTrs,
    inferSigFromRules,
    vars,
    varsCondition,
    ruleToCRule,
    trsToOrientedCTrs,
    orientedCTrsToTrs,
) where

import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.Map.Strict as M
import TRSConversion.Problem.Common.Term (Term (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig)
import qualified TRSConversion.Problem.Common.Term as Term
import Data.Containers.ListUtils (nubOrd)
import qualified TRSConversion.Problem.Trs.Trs as Trs

data CondType = Oriented | Join | SemiEquational
    deriving (Eq, Show)

data Condition f v = (Term f v) :== (Term f v)
    deriving (Eq, Show)

mapCondition :: (f -> f') -> (v -> v') -> Condition f v -> Condition f' v'
mapCondition f v (t1 :== t2) = Term.map f v t1 :== Term.map f v t2

data CRule f v = CRule
    { lhs :: Term f v
    -- ^ The left-hand side of the rule
    , rhs :: Term f v
    -- ^ The right-hand side of the rule
    , conditions :: [Condition f v]
    -- ^ Conditions
    }
    deriving (Eq, Show)


mapCRule :: (f -> f') -> (v -> v') -> CRule f v -> CRule f' v'
mapCRule f v crule = CRule { lhs = Term.map f v (lhs crule)
                           , rhs = Term.map f v (rhs crule)
                           , conditions = mapCondition f v <$> conditions crule
                           }

ruleToCRule :: Trs.Rule f v -> CRule f v
ruleToCRule Trs.Rule{Trs.lhs = l, Trs.rhs = r} = CRule {lhs = l, rhs = r, conditions = []}

cRuleToRule :: CRule f v -> Maybe (Trs.Rule f v)
cRuleToRule CRule{lhs = l, rhs = r, conditions = []} = Just $ Trs.Rule {Trs.lhs = l, Trs.rhs = r}
cRuleToRule _ = Nothing

data CTrs f v = CTrs
    { conditionType :: CondType
    , rules :: IntMap [CRule f v]
    -- ^ Mapping index to CTrs
    , signature :: TrsSig f
    -- ^ The signature (function symbols and corresponding sorts) for the MSTRS
    , numSystems :: Int
    -- ^ number of rewrite systems
    }
    deriving (Show, Eq)

mapCTrs :: (f -> f') -> (v -> v') -> CTrs f v -> CTrs f' v'
mapCTrs f v ctrs = ctrs { rules = fmap (mapCRule f v) <$> rules ctrs
                        ,  signature = fmap f (signature ctrs)
                        }

trsToOrientedCTrs :: Trs.Trs f v -> CTrs f v
trsToOrientedCTrs Trs.Trs {Trs.rules = rs, Trs.signature = sig, Trs.numSystems = n} =
  CTrs { rules = map ruleToCRule <$> rs
       , conditionType = Oriented
       , signature = sig
       , numSystems = n
       }

orientedCTrsToTrs :: CTrs f v -> Maybe (Trs.Trs f v)
orientedCTrsToTrs CTrs {rules = rs, signature = sig, numSystems = n, conditionType = condType}
  | condType /= Oriented = Nothing
  | otherwise = do
      rs' <- traverse (traverse cRuleToRule) rs
      pure $ Trs.Trs { Trs.rules = rs'
                     , Trs.signature = sig
                     , Trs.numSystems = n
                     }

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

vars :: Ord v => [CRule f v] -> [v]
vars rs = nubOrd $ concatMap ruleVars rs
 where
   ruleVars rl =
     Term.vars (lhs rl) ++ Term.vars (rhs rl) ++ concatMap varsCondition (conditions rl)

varsCondition :: Condition f a -> [a]
varsCondition (l :== r) = Term.vars l ++ Term.vars r
