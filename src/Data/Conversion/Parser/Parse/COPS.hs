{-# LANGUAGE FlexibleContexts#-}
module Data.Conversion.Parser.Parse.COPS
  ( parseIO
  )
where


import qualified Data.Conversion.Problem.TRS as TRS
import Data.Conversion.Problem.TRS (TRS(..))
import qualified Data.Conversion.Problem.Rule as Rule
import Data.Conversion.Problem.Rule (Rule(..))
import Data.Conversion.Problem.Term (parseTerm, identWST)

import Data.List (partition, union)
import Data.Maybe (isJust)
import Prelude hiding (lex, catch)
import Control.Exception (catch)
import Control.Monad.Error
import Control.Monad (liftM, liftM3)
import Text.Parsec hiding (parse)
import System.IO (readFile)
import Data.Conversion.Utils (lex, par, ident)

data ProblemParseError = UnknownParseError String 
                       | SomeParseError ParseError deriving (Show)

instance Error ProblemParseError where strMsg = UnknownParseError

parseIO :: String -> IO (TRS String String)
parseIO str = case fromString str of
                    Left err -> do { putStrLn "following error occured:"; print err; mzero }
                    Right t  -> return t


fromString :: String -> Either ProblemParseError (TRS String String)
fromString = fromCharStream "supplied string"

fromCharStream :: (Stream s (Either ProblemParseError) Char)
                   => SourceName -> s -> Either ProblemParseError (TRS String String)
fromCharStream sourcename input =
  case runParserT parse initialState sourcename input of
    Right (Left e)  -> Left $ SomeParseError e
    Right (Right p) -> Right p
    Left e          -> Left e
  where initialState = TRS.TRS { TRS.rules      = [] ,
                                      TRS.variables  = [] ,
                                      TRS.signature  = Nothing,
                                      TRS.comment    = Nothing }

-- | Type synonym for a TRS with @string@ function symbols and variables
type ParserState = TRS String String

type WSTParser s a = ParsecT s ParserState (Either ProblemParseError) a


-- | Return a parser for the variables identified so far during parsing
parsedVariables :: WSTParser s [String]
parsedVariables = fmap TRS.variables getState

parse :: (Stream s (Either ProblemParseError) Char) => WSTParser s (TRS String String)
parse = spaces >> parseDecls >> eof >> getState where
  parseDecls = many1 parseDecl
  parseDecl =  decl "VAR"       parseVars   (\ e p -> p {TRS.variables = e `union` TRS.variables p})
           <|> decl "SIG"       parseSig    (\ e p -> p {TRS.signature = maybeAppend TRS.signature e p})
           <|> decl "RULES"     parseRules  (\ e p -> p {TRS.rules   = e  -- qqjf multiple RULES blocks?
                                                         })
           <|> (decl "COMMENT"   parseComment   (\ e p -> p {TRS.comment = maybeAppend TRS.comment e p}) <?> "comment")
           <|> (par parseComment >>= modifyState . (\ e p -> p {TRS.comment = maybeAppend TRS.comment e p}) <?> "comment")
  decl name p f = try (par $ do
      _ <- lex $ string name
      r <- p
      modifyState $ f r) <?> (name ++ " block")
  maybeAppend fld e p = Just $ maybe [] id (fld p) ++ e


-- | Parser to extract variables from a VARS block of the COPS TRS format
--   For example, from a block @(VAR x y zs)@ we wnt to extract a list @["x","y","zs"]@
parseVars :: (Stream s (Either ProblemParseError) Char) => WSTParser s [String]
parseVars = many (lex identWST) 

-- | Parser to extract the signature from a SIG block of the COPS TRS format
--   For example, from a block @(SIG (f 2) (a 0) (b 1))@ we wnt to extract a list @[("f",2),("a",0),("b",1)]@
parseSig :: (Stream s (Either ProblemParseError) Char) => WSTParser s [(String,Int)]
parseSig = many fundecl
    where
        fundecl = par (do
            fsym  <- lex identWST
            arity <- lex (read <$> many1 digit)
            return (fsym,arity))

-- | Parser to extract the 'Rule's from a RULES block of the COPS TRS format
--   Expects format @lhs -> rhs@ and calls the 'Term' parser 'parseTerm' on each side of the rule
parseRules :: (Stream s (Either ProblemParseError) Char) => WSTParser s [Rule String String]
parseRules = do vars <- parsedVariables
                many $ parseRule vars 
  where parseRule vs = do l <- parseTerm vs
                          _ <- lex $ string "->"
                          r <- parseTerm vs
                          return $ Rule {lhs = l, rhs = r}

-- | Parser to extract get comments as a @String@ from a @COMMENT@ block of the COPS TRS format
parseComment :: (Stream s (Either ProblemParseError) Char) => WSTParser s String
parseComment = withParens <|> liftM2 (++) idents parseComment <|> return ""
  where idents = many1 (noneOf "()")
        withParens = do _ <- char '('
                        pre <- parseComment
                        _ <- char ')'
                        suf <- parseComment
                        return $ "(" ++ pre ++ ")" ++ suf