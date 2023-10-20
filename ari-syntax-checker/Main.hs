{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr)
import Text.Megaparsec (PosState (..), defaultTabWidth, initialPos)
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Error.Builder as PE

import qualified TRSConversion.Parse.ARI.Problem as ARI
import TRSConversion.Parse.ARI.Utils (FunSymb, SortSymb, VarSymb)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Parse.Utils (Token (..), parseIO)
import qualified TRSConversion.Problem.CSCTrs.CSCTrs as CSCTrs
import qualified TRSConversion.Problem.CSTrs.CSTrs as CSTrs
import TRSConversion.Problem.CTrs.CTrs (orientedCTrsToTrs)
import qualified TRSConversion.Problem.CTrs.CTrs as CTrs
import qualified TRSConversion.Problem.CTrs.Infeasibility as Inf
import TRSConversion.Problem.Common.Term (Term (..), vars)
import qualified TRSConversion.Problem.MsTrs.MsTrs as MsTrs
import TRSConversion.Problem.Problem (System (..), system)
import qualified TRSConversion.Problem.Problem as Prob
import qualified TRSConversion.Problem.Trs.Trs as Trs

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

data Result a = Fail (Maybe ErrInfo) a | Succeed
    deriving (Show)

data ErrInfo = ErrInfo
    { start :: Int
    , len :: Int
    , errToken :: Text
    , errorMsg :: String
    }
    deriving (Show, Eq, Ord)

instance ShowErrorComponent ErrInfo where
    showErrorComponent :: ErrInfo -> String
    showErrorComponent = errorMsg

    errorComponentLen :: ErrInfo -> Int
    errorComponentLen = len

errorBundleFromErrInfo :: ErrInfo -> FilePath -> s -> ParseErrorBundle s ErrInfo
errorBundleFromErrInfo errInf filename fileContent =
    ParseErrorBundle
        { bundlePosState =
            PosState
                { pstateInput = fileContent
                , pstateOffset = 0
                , pstateSourcePos = initialPos filename
                , pstateTabWidth = defaultTabWidth
                , pstateLinePrefix = ""
                }
        , bundleErrors =
            NonEmpty.fromList [PE.errFancy (start errInf) (PE.fancy $ ErrorCustom errInf)]
        }

errInfoFromToken :: Token a -> String -> ErrInfo
errInfoFromToken tok errMsg =
    ErrInfo
        { start = tokenOffset tok
        , len = tokenLength tok
        , errToken = tokenText tok
        , errorMsg = errMsg
        }

instance Semigroup (Result a) where
    (<>) :: Result a -> Result a -> Result a
    a@(Fail _ _) <> _ = a
    Succeed <> b = b

instance Monoid (Result a) where
    mappend :: Result a -> Result a -> Result a
    mappend = (<>)

    mempty :: Result a
    mempty = Succeed

runApp :: FilePath -> IO ()
runApp fp = do
    fileContent <- Text.readFile fp
    problem <- parseIO (ARI.toParser ARI.parseProblem') fp fileContent
    case checkSem (system problem) of
        Fail errInfo err -> do
            hPutStrLn stderr "ERROR:"
            case errInfo of
                Nothing -> hPutStrLn stderr err
                Just errInf ->
                    let errBundle = errorBundleFromErrInfo errInf fp fileContent
                     in hPutStr stderr $ errorBundlePretty errBundle
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

checkSem :: System FunSymb VarSymb SortSymb -> Result String
checkSem (Trs sys) = checkTrs sys
checkSem (CTrs sys) = checkCTrs sys
checkSem (MSTrs sys) = checkMSTrs sys
checkSem (CSTrs sys) = checkCSTrs sys
checkSem (CSCTrs sys) = checkCSCTrs sys
checkSem (Infeasibility sys) = checkInfeasibility sys

--------------------------------------------------------------------------------
------ TRSs

checkTrs :: Trs.Trs FunSymb VarSymb -> Result String
checkTrs Trs.Trs{Trs.rules = rs, Trs.signature = sig} =
    mconcat
        [ checkSignature sig
        , foldMap (foldMap checkRule) rs
        ]

checkSignature :: Trs.TrsSig FunSymb -> Result String
checkSignature (Trs.FunSig sig) = uniqueSymbols sig

uniqueSymbols :: [Trs.Sig FunSymb] -> Result String
uniqueSymbols = go Set.empty
  where
    go _ [] = Succeed
    go st (Trs.Sig f _ : xs)
        | f `Set.member` st =
            let errMsg = "the function symbol is declared multiple times"
             in Fail (Just (errInfoFromToken f errMsg)) errMsg
        | otherwise = go (Set.insert f st) xs

checkRule :: Trs.Rule FunSymb VarSymb -> Result String
checkRule Trs.Rule{Trs.lhs = l, Trs.rhs = r} =
    case l of
        Var v ->
            let errMsg = "rules may not have a variable left-hand side"
             in Fail (Just (errInfoFromToken v errMsg)) errMsg
        _
            | (v : _) <- Set.toList $ rVars `Set.difference` lVars ->
                let errMsg = "all variables on the right of a rule must appear on the left"
                 in Fail (Just (errInfoFromToken v errMsg)) errMsg
        _ -> Succeed
  where
    lVars = Set.fromList (vars l)
    rVars = Set.fromList (vars r)

--------------------------------------------------------------------------------
------ CTRSs

checkCTrs :: CTrs.CTrs FunSymb VarSymb -> Result String
checkCTrs CTrs.CTrs{CTrs.rules = rs, CTrs.signature = sig} =
    mconcat
        [ checkSignature sig
        , foldMap (foldMap checkCRule) rs
        ]

checkCRule :: CTrs.CRule FunSymb VarSymb -> Result String
checkCRule CTrs.CRule{CTrs.lhs = l} =
    case l of
        Var v ->
            let errMsg = "rules may not have a variable left-hand side"
             in Fail (Just (errInfoFromToken v errMsg)) errMsg
        _ -> Succeed

--------------------------------------------------------------------------------
------ MSTrs

checkMSTrs :: MsTrs.MsTrs FunSymb VarSymb SortSymb -> Result String
checkMSTrs MsTrs.MsTrs{MsTrs.rules = rs, MsTrs.signature = sig} =
    mconcat
        [ checkMSSignature sig
        , foldMap (foldMap checkRule) rs
        ]

checkMSSignature :: [MsTrs.MsSig FunSymb SortSymb] -> Result String
checkMSSignature = go Set.empty
  where
    go _ [] = Succeed
    go st (MsTrs.MsSig f _ : xs)
        | f `Set.member` st =
            let errMsg = "function symbol is declared multiple times"
             in Fail (Just (errInfoFromToken f errMsg)) errMsg
        | otherwise = go (Set.insert f st) xs

--------------------------------------------------------------------------------
------ CSTrs
checkCSTrs :: CSTrs.CSTrs FunSymb VarSymb -> Result String
checkCSTrs CSTrs.CSTrs{CSTrs.rules = rs, CSTrs.signature = sig} =
    mconcat
        [ checkSignature sig
        , foldMap (foldMap checkRule) rs
        ]

--------------------------------------------------------------------------------
------ CSCTrs
checkCSCTrs :: CSCTrs.CSCTrs FunSymb VarSymb -> Result String
checkCSCTrs CSCTrs.CSCTrs{CSCTrs.ctrs = ctrs} = checkCTrs ctrs

--------------------------------------------------------------------------------
------ Infeasibility

checkInfeasibility :: Inf.Infeasibility FunSymb VarSymb -> Result String
checkInfeasibility Inf.Infeasibility{Inf.ctrs = ctrs, Inf.isTrs = isTrs}
    | isTrs = case orientedCTrsToTrs ctrs of
        Nothing -> Fail Nothing "the system is not a TRS"
        Just trs -> checkTrs trs
    | otherwise = checkCTrs ctrs
