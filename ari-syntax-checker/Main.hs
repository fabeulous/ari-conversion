{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import Prettyprinter (Pretty, indent, vsep)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified TRSConversion.Parse.ARI.Problem as ARI
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Parse.Utils (parseIO)
import qualified TRSConversion.Problem.CSCTrs.CSCTrs as CSCTrs
import qualified TRSConversion.Problem.CSTrs.CSTrs as CSTrs
import qualified TRSConversion.Problem.CTrs.CTrs as CTrs
import qualified TRSConversion.Problem.CTrs.Infeasibility as Inf
import TRSConversion.Problem.Common.Term (Term (..), vars)
import qualified TRSConversion.Problem.MsTrs.MsTrs as MsTrs
import TRSConversion.Problem.Problem (System, ParsedSystem (..), system)
import qualified TRSConversion.Problem.Problem as Prob
import qualified TRSConversion.Problem.Trs.Trs as Trs
import TRSConversion.Unparse.CTrs (unparseAriCRules)
import TRSConversion.Unparse.Problem.MsSig (unparseAriMsSig)
import TRSConversion.Unparse.Problem.Rule (unparseAriRule)
import TRSConversion.Unparse.Problem.TrsSig (unparseAriSigs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> runApp filename
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ usage name
            exitFailure

usage :: String -> String
usage name = "Usage: " ++ name ++ " FILE"

data Result a = Fail a | Succeed
    deriving (Eq, Ord, Show)

instance Semigroup (Result a) where
    (<>) :: Result a -> Result a -> Result a
    Fail a <> _ = Fail a
    Succeed <> b = b

instance Monoid (Result a) where
    mappend :: Result a -> Result a -> Result a
    mappend = (<>)

    mempty :: Result a
    mempty = Succeed

runApp :: FilePath -> IO ()
runApp fp = do
    fileContent <- Text.readFile fp
    problem <- parseIO (ARI.toParser ARI.parseProblem) fp fileContent
    case checkSem (system problem) of
        Fail err -> do
            hPutStrLn stderr "ERROR:"
            hPutStrLn stderr err
            exitFailure
        Succeed -> do
            putStr $ case Prob.system problem of
                (Prob.Trs _) -> "trs"
                (Prob.MSTrs _) -> "mstrs"
                (Prob.CTrs _) -> "ctrs"
                (Prob.CSTrs _) -> "cstrs"
                (Prob.CSCTrs _) -> "csctrs"
                (Prob.Infeasibility _) -> "infeasibility"
            putStrLn " successfully parsed"
            exitSuccess

checkSem :: System -> Result String
checkSem (Trs sys) = checkTrs sys
checkSem (CTrs sys) = checkCTrs sys
checkSem (MSTrs sys) = checkMSTrs sys
checkSem (CSTrs sys) = checkCSTrs sys
checkSem (CSCTrs sys) = checkCSCTrs sys
checkSem (Infeasibility sys) = checkInfeasibility sys

--------------------------------------------------------------------------------
------ TRSs

checkTrs :: (Ord v, Pretty f, Pretty v, Ord f) => Trs.Trs f v -> Result String
checkTrs Trs.Trs{Trs.rules = rs, Trs.signature = sig} =
    mconcat
        [ checkSignature sig
        , foldMap (foldMap checkRule) rs
        ]

checkSignature :: (Ord f, Pretty f) => Trs.TrsSig f v -> Result String
checkSignature (Trs.FunSig sig) = uniqueSymbols sig

uniqueSymbols :: (Ord f, Pretty f) => [Trs.Sig f] -> Result String
uniqueSymbols = go Set.empty
  where
    go _ [] = Succeed
    go st (sigLine@(Trs.Sig f _) : xs)
        | f `Set.member` st =
            Fail
                . show
                $ vsep
                    [ "the function symbol"
                    , indent 4 $ unparseAriSigs [sigLine]
                    , "is declared multiple times"
                    ]
        | otherwise = go (Set.insert f st) xs

checkRule :: (Ord v, Pretty f, Pretty v) => Trs.Rule f v -> Result String
checkRule rule@Trs.Rule{Trs.lhs = l, Trs.rhs = r} =
    case l of
        Var _ ->
            Fail
                . show
                $ vsep
                    [ "the rule"
                    , indent 4 $ unparseAriRule 1 rule
                    , "may not have a variable left-hand side"
                    ]
        _
            | not (rVars `Set.isSubsetOf` lVars) ->
                Fail
                    . show
                    $ vsep
                        [ "the rule"
                        , indent 4 $ unparseAriRule 1 rule
                        , "violates the variable condition"
                        ]
        _ -> Succeed
  where
    lVars = Set.fromList (vars l)
    rVars = Set.fromList (vars r)

--------------------------------------------------------------------------------
------ CTRSs

checkCTrs :: (Pretty f, Pretty v, Ord f) => CTrs.CTrs f v -> Result String
checkCTrs CTrs.CTrs{CTrs.rules = rs, CTrs.signature = sig} =
    mconcat
        [ checkSignature sig
        , foldMap (foldMap checkCRule) rs
        ]

checkCRule :: (Pretty f, Pretty v) => CTrs.CRule f v -> Result String
checkCRule rule@CTrs.CRule{CTrs.lhs = l} =
    case l of
        Var _ ->
            Fail
                . show
                $ vsep
                    [ "the rule"
                    , indent 4 $ unparseAriCRules 1 [rule]
                    , "may not have a variable left-hand side"
                    ]
        _ -> Succeed

--------------------------------------------------------------------------------
------ MSTrs

checkMSTrs :: MsTrs.MsTrs String String String -> Result String
checkMSTrs MsTrs.MsTrs{MsTrs.rules = rs, MsTrs.signature = sig} =
    mconcat
        [ checkMSSignature sig
        , foldMap (foldMap checkRule) rs
        ]

checkMSSignature :: [MsTrs.MsSig String String] -> Result String
checkMSSignature = go Set.empty
  where
    go _ [] = Succeed
    go st (sigLine@(MsTrs.MsSig f _) : xs)
        | f `Set.member` st =
            Fail
                . show
                $ vsep
                    [ "the function symbol"
                    , indent 4 $ unparseAriMsSig [sigLine]
                    , "is declared multiple times"
                    ]
        | otherwise = go (Set.insert f st) xs

--------------------------------------------------------------------------------
------ CSTrs
checkCSTrs :: CSTrs.CSTrs String String -> Result String
checkCSTrs CSTrs.CSTrs{CSTrs.rules = rs, CSTrs.signature = sig} =
    mconcat
        [ checkSignature sig
        , foldMap (foldMap checkRule) rs
        ]

--------------------------------------------------------------------------------
------ CSCTrs
checkCSCTrs :: CSCTrs.CSCTrs String String -> Result String
checkCSCTrs CSCTrs.CSCTrs{CSCTrs.ctrs = ctrs} = checkCTrs ctrs

--------------------------------------------------------------------------------
------ Infeasibility

checkInfeasibility :: Inf.Infeasibility String String -> Result String
checkInfeasibility Inf.Infeasibility{Inf.ctrs = ctrs} = checkCTrs ctrs
