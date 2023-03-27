module Main (main) where

import Control.Monad (unless)
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Prettyprinter (Doc)
import System.Console.GetOpt (
  ArgDescr (NoArg, ReqArg),
  ArgOrder (Permute),
  OptDescr (..),
  getOpt,
  usageInfo,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, IOMode (WriteMode), hClose, hPrint, hPutStrLn, openFile, stderr, stdout)
import Text.Megaparsec (choice, eof, errorBundlePretty, parse, try, (<?>))

import qualified Data.Conversion.Parse.ParseMsTrs as P
import qualified Data.Conversion.Parse.ParseTrs as P
import Data.Conversion.Parse.Utils (Parser)
import Data.Conversion.Problem.MsTrs.MsTrs (MsTrs)
import Data.Conversion.Problem.Trs.Trs (Trs)
import Data.Conversion.Unparse.UnparseMsTrs (unparseAriMsTrs, unparseCopsMsTrs)
import Data.Conversion.Unparse.UnparseTrs (unparseAriTrs, unparseCopsTrs)

data Format
  = COPS
  | ARI

data Config = Config
  { target :: Format
  , source :: Format
  , outputHandle :: Handle
  }

defaultConfig :: Config
defaultConfig =
  Config
    { source = COPS
    , target = ARI
    , outputHandle = stdout
    }

options :: [OptDescr (Config -> IO Config)]
options =
  [ Option
      ['f']
      ["from"]
      ( ReqArg
          ( \s c -> case toUpper <$> s of
              "ARI" -> pure $ c{source = ARI}
              "COPS" -> pure $ c{source = COPS}
              _ -> error $ "Error: '" ++ s ++ "' is not a known source"
          )
          "FORMAT"
      )
      "source format"
  , Option
      ['t']
      ["to"]
      ( ReqArg
          ( \s c -> case toUpper <$> s of
              "ARI" -> pure $ c{target = ARI}
              "COPS" -> pure $ c{target = COPS}
              _ -> error $ "Error: '" ++ s ++ "' is not a known target"
          )
          "FORMAT"
      )
      "target format"
  , Option
      ['o']
      ["output"]
      ( ReqArg
          ( \s c -> do
              fileHandle <- openFile s WriteMode
              pure $ c{outputHandle = fileHandle}
          )
          "FILE"
      )
      "write output to FILE"
  , Option
      ['h']
      ["help"]
      ( NoArg
          ( \_ -> do
              usage stdout
              exitSuccess
          )
      )
      "print this message"
  ]

usage :: Handle -> IO ()
usage handle = do
  execName <- getProgName
  hPutStrLn handle (usageInfo (header execName) options)
 where
  header execName =
    unlines
      [ "Usage: " ++ execName ++ " [OPTIONS] FILE"
      , "Convert problems involving term-rewrite systems between formats."
      , ""
      , "It is mandatory to give a source format (-f) and target format (-t)."
      , "Moreover the input FILE must contain a problem in the source format."
      , "The following FORMATs are supported: COPS, ARI."
      , "The problem type is inferred from the input and may be: TRS, MSTRS."
      , ""
      , "OPTIONS:"
      ]

{- | @trs-conversion-exe@ entry point. Can be run by calling
> stack build
> stack exec trs-conversion-exe

Currently just contains some simple hard-coded examples to illustrate
parsing and unparsing functionality.
-}
main :: IO ()
main = do
  args <- getArgs
  let (opts, nonOpts, errs) = getOpt Permute options args
  unless (null errs) $ do
    hPutStrLn stderr $ "Error(s) parsing arguments:\n" ++ show errs
    usage stderr
    exitFailure
  conf <- foldl (>>=) (pure defaultConfig) opts
  case nonOpts of
    [inputFile] -> runApp conf inputFile
    _ -> do
      hPutStrLn stderr "Error: expected exactly one input file"
      usage stderr
      exitFailure

runApp :: Config -> FilePath -> IO ()
runApp config inputFile = do
  fileContents <- Text.readFile inputFile

  problem <- case source config of
    COPS -> parseIO copsParser "COPS TRS" fileContents
    ARI -> parseIO ariParser "ARI TRS" fileContents

  doc <- case problem of
    TTrs trs -> case target config of
      COPS -> unparseIO unparseCopsTrs trs
      ARI -> unparseIO unparseAriTrs trs
    TMSTrs trs -> case target config of
      COPS -> unparseIO unparseCopsMsTrs trs
      ARI -> unparseIO (pure . unparseAriMsTrs) trs

  hPrint (outputHandle config) doc

  hClose (outputHandle config)

data Problem = TTrs (Trs String String) | TMSTrs (MsTrs String String String)

copsParser :: Parser Problem
copsParser =
  choice
    [ TTrs <$> try P.parseCopsTrs
    , TMSTrs <$> try P.parseCopsMsTrs
    ]
    <?> "COPS problem"

ariParser :: Parser Problem
ariParser =
  choice
    [ TTrs <$> try P.parseAriTrs
    , TMSTrs <$> try P.parseAriMsTrs
    ]
    <?> "ARI problem"

parseIO :: Parser a -> String -> Text -> IO a
parseIO p inpName inp =
  case parse (p <* eof) inpName inp of
    Left err -> ioError $ userError (errorBundlePretty err)
    Right trs -> return trs

-- | Takes an unparsing function @up@ as an argument and wraps the result in the IO monad
unparseIO :: (a -> Either String (Doc ann)) -> a -> IO (Doc ann)
unparseIO up input = case up input of
  Left err -> ioError $ userError err
  Right copsMsTrs -> return copsMsTrs
