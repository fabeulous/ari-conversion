{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (guard)
import qualified Control.Monad.Fail (fail)
import Data.Rewriting.Term.Type
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    anySingleBut,sepBy1,
    between,
    eof,
    errorBundlePretty,
    lookAhead,
    many,
    manyTill,
    manyTill_,
    noneOf,
    notFollowedBy,
    oneOf,
    parse,
    parseTest,
    runParserT,
    satisfy,
    sepBy,
    skipManyTill,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
 

type Parser = Parsec Void Text
 
type Vars = [String]

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

-- | lexeme is a lexeme that consumes all trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a term given a list of variables by calling 'parseVariable' and 'parseFunApplication'
-- Tries to parse the expression as a function application first
parseTerm :: Vars -> Parser (Term String String)
parseTerm vs = try (stripOuterParens    *> parseTerm vs )
     <|> try (parseVariable vs) 
    <|> try (parseFunApplication vs) 
    <|>  try parseConstant  
    -- <* eof
    where 
        parseConstant ::Parser (Term String String)
        parseConstant = do
            fsym <- lexeme (some allowedFunVarChars) -- <* eof
            return (Fun fsym []) 
  --input <- many anySingle
  --return (Var input)

-- | Parse a single variable
--   Currently requires the first character to be a letter
-- Uses @lexeme@ to comsume trailing whitespace
parseVariable :: Vars -> Parser (Term f String)
parseVariable vs = do
  varStr <- (:) <$> letterChar <*> lexeme (many allowedFunVarChars) <?> "variable"
  guard (varStr `elem` vs)
  return (Var varStr)

{-
-- var <- (:) <$> letterChar <*> lexeme (many allowedFunVarChars) <?> "variable"
if var `elem` vs
  then return (Var var)
  else fail ("Var " ++ var ++ " not in variable set " ++ show vs)-}

-- | Parse a function application and return a 'Term'
-- Assumes that everything until the first @'('@ is a function symbol
-- For example, "f(x,y, g(z))"
parseFunApplication :: Vars -> Parser (Term String String)
parseFunApplication vs =
  do 
    fsym <- parseFunSymbol
    -- Remove outer parentheses
    argsStr <- stripOuterParens

    -- Recursively parse function arguments
    let argsList = case parse (parseFunArgs vs <* eof) "" (pack argsStr) of
          Left bundle -> fail "aaaa" -- errorBundlePretty bundle
          Right args -> args 
    rem <- many anySingle -- Consume remainder of input (qqjf) 
    return (Fun fsym argsList)

-- \| Recursively strip outer parentheses of a function application, even if nested
stripOuterParens ::  Parser String
stripOuterParens   = try (aux  *> aux) <|> aux where
        aux ::  Parser String
        aux = do
            (argsStr, _) <-  (char '(') *> lookAhead (manyTill_ anySingle (try (char ')'  <* eof)))  
            return argsStr

-- | Parse a function symbol either until the first '(' or until the end of the string
parseFunSymbol :: Parser String
parseFunSymbol = try (funSymChars <* lookAhead (char '(')) <|> (funSymChars <* eof) <?> "function symbol"
  where
    funSymChars :: Parser String
    funSymChars = lexeme (some allowedFunVarChars)

-- | Parse function arguments
-- Expects balanced parentheses and comma-separated values
-- Returns a list of the arguments. Also accepts an empty string or just whitespace, corresponding to an empty arguments list.
-- e.g. "(a, b, c)" or "(a, b(c))"
parseFunArgs :: Vars -> Parser [Term String String]
parseFunArgs vs = do   
    terms <-  parseArgList <* eof {- <|> emptyArgList-}  -- Parse arguments between parentheses  
    return terms
    --return ([Var (show argsList)])
    --let eitherTerms = [parse (parseTerm vs) "" (pack argsStr) | argsStr <- argsList]
    --let terms = case parse (parseTerm vs) "" (pack argsList) of
    --      Left bundle -> fail "aaaaaa" -- errorBundlePretty bundle
    --      Right args -> args 
    -- input <- many anySingle
    --let terms = [t | Right t <- eitherTerms]
    --([Var s | s <- argsList])
  where
    funCharConsumer :: Parser Char
    funCharConsumer = try (allowedFunVarChars) <|> (char '(' *> skipManyTill allowedFunVarChars (char ')'))
    -- try parseArgList <|> emptyArgList -- <* eof

    -- input <- many anySingle
    -- return ([Var input])

    -- try parseArgList <|> emptyArgList <* eof -- Parse arguments between parentheses
    -- input <- many anySingle
    -- return [Var input] 
    -- \| Parser for an empty arguments list
    emptyArgList :: Parser [Term String String]
    emptyArgList = lexeme (eof) *> return []
    -- \| Parser for a non-empty arguments list
    parseArgList :: Parser [Term String String]
    --parseArgList = (lookAhead (many funCharConsumer)) `sepBy` (char ',')
    parseArgList = (parseTerm vs ) `sepBy` (char ',') --char '(' *> (parseTerm vs) `sepBy` (char ',') -- <* char ')' <* eof
    --parseConst :: Parser [Term String String]
    --parseConst = (\fsym -> [Fun fsym []]) <$> parseFunSymbol
    -- parseArgList = stripOuterParens *> --
    --termParser :: Parser ([Term String String])
    --termParser = manyTill (parseTerm vs) (char ')')

-- | Parser for characters allowed after the first character of variables and for function symbols.
--   Currently allows any character except for '(', ')', ',', and whitespace.
--   TODO: block all whitespace and special characters, not just a single space
allowedFunVarChars :: Parser Char
allowedFunVarChars = noneOf ['(', ')', ' ', ',']

t1 = parseTest (parseVariable ["x", "y"] :: Parser (Term String String)) "x"

t2 = parseTest (parseVariable ["x", "y"] :: Parser (Term String String)) "xz"

t3 = parseTest (parseVariable ["x", "y"] :: Parser (Term String String)) "xt"

t4 = parseTest (parseVariable ["x", "y"] :: Parser (Term String String)) "y "

t5 = parseTest (parseVariable ["x", "y"] :: Parser (Term String String)) " y"

t6 = parseTest (parseVariable ["x", "y"] :: Parser (Term String String)) "y x"

t7 = parseTest (parseVariable ["x'", "y"] :: Parser (Term String String)) "x'"

t8 = parseTest parseFunSymbol "x'"

t9 = parseTest parseFunSymbol "x' "

t10 = parseTest parseFunSymbol " x'"

{-p1 = parseTest parseFunArgs "(abd)"

p2 = parseTest parseFunArgs "(abd)e"

p3 = parseTest parseFunArgs "(abd))"

p5 = parseTest parseFunArgs "(abd))"

p6 = parseTest parseFunArgs "(a,b, c)"

p7 = parseTest parseFunArgs "(a, b(d))"-}

vars :: Vars
vars = ["x", "y", "z"]

-- Working
g4 = parseTest (parseTerm vars <* eof) "x"
g5 = parseTest (parseTerm vars <* eof) "c"
g0 = parseTest (parseTerm vars <* eof) "f(x,y,z)"
g7 = parseTest (parseTerm vars <* eof) ",c" 
g8 = parseTest (parseTerm vars <* eof) "c," 
g1 = parseTest (parseTerm vars) "f(c,y,z)"
g6 = parseTest (parseTerm vars) "f()" 
g4a = parseTest (parseTerm vars) "(x)"
g5a = parseTest (parseTerm vars) "(c)"
g5b = parseTest (parseTerm vars) "((c))"
g5c = parseTest (parseTerm vars) "((c)"
g5d = parseTest (parseTerm vars) "(c))"

g9 = parseTest (parseFunApplication vars) "f(c,asdas,f(g))" 
g9a = parseTest (parseFunArgs vars) "c,asdas,f(g)" 
g2 = parseTest (parseTerm vars) "f(x,y,b(d))"

g3 = parseTest (parseTerm vars) "f(x,y,b(d,e))"


parseArgList2 ::   Parser [String]
parseArgList2   = do
    out <- (many allowedFunVarChars) `sepBy` (char ',') 
    return out

parseArgList3 :: Vars -> Parser ([Term String String ])
parseArgList3 vs = do
        let outTerms = case parse (parseTerm vs) "" "c,y,z" of
             Left bundle -> fail "aaa" -- putStr (errorBundlePretty bundle)
             Right ts -> return ts
        return outTerms
parseArgList :: Vars -> Parser [Term String String]
parseArgList vs = (try (parseTerm vs) <|> fail "aa") `sepBy` (char ',')
parseArgListNew :: Vars ->Parser [Term String String]
parseArgListNew vs = do
    matchingTermStrs <-  many anySingle -- parseArg2 --(try parseArg <|> fail "aa") 
    let terms = case parse parseTerms "" (pack matchingTermStrs) of
         Left bundle -> [Var "ERROR"]--putStr (errorBundlePretty bundle)
         Right ts -> ts
    return terms
    where 
        parseTerms :: Parser [Term String String]
        parseTerms = do
            _ <- lookAhead (many allowedFunVarChars)
            out <- many anySingle
            return [Var out]
            --output <- (parseTerm vs) `sepBy` char ','
            --return output
        parseArg1 :: Parser [Term String String]
        parseArg1 = (parseTerm vs) `sepBy` (char ',')
        parseArg2 :: Parser [String]
        parseArg2 = (many allowedFunVarChars) `sepBy` (char ',')

parser3 :: Vars -> Parser (Term String String)
parser3 vs = (parseTerm vs)  
parser4 :: Vars -> Parser [Term String String]
parser4 vs = ((parseTerm vs)   `sepBy` (char ','))
-- q2 = parseTest (parseArgList2   ) "c,y,z"
q1 = parseTest (parseArgListNew vars  ) "c,y,z" 
q3 = parseTest (parser3  vars) "c,y,z" 
q4 = parseTest (parser4  vars <* eof) "c,x" 
