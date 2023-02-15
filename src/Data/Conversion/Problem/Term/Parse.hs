{-# LANGUAGE FlexibleContexts#-}
module Data.Conversion.Problem.Term.Parse (
    parseTerm,
    identWST
) where

import Data.Conversion.Utils (lex, par, ident)
import Prelude hiding (lex)
import Control.Monad (liftM2)
import Data.Conversion.Problem.Term.Type (Term(..))
import Text.Parsec hiding (parse)
import Data.Rewriting.Term.Parse (parseFun, parseVar)


-- | @parseTerm xs@ is a parser for terms following the following conventions:
--
--     * every @Char@ that is neither a white space (according to 'Data.Char.isSpace') nor one of '@(@',
--        '@)@', or '@,@', is considered a letter.
--
--     * An identifier is a non-empty sequence of letters and it is treated as variable iff it is contained in @xs@.
-- qqjf: what about numbers in function names?
parseTerm :: Stream s m Char => [String] -> ParsecT s u m (Term String String)
parseTerm xs = parse (parseFun identWST) (parseVar identWST xs)


-- | @parse fun var@ is a parser for terms, where @fun@ and @var@ are
-- parsers for function symbols and variables, respectively. The @var@ parser
-- has a higher priority than the @fun@ parser. Hence, whenever @var@
-- succeeds, the token is treated as a variable.
--
-- Note that the user has to take care of handling trailing white space in
-- @fun@ and @var@.
parse :: Stream s m Char => ParsecT s u m f -> ParsecT s u m v
    -> ParsecT s u m (Term f v)
parse fun var = term <?> "term" where
    term = try (fmap Var var) <|> liftM2 Fun fun args
    args =  par (sepBy term (lex $ char ',')) <|> return []
 

-- | Parser which breaks on characters @(@, @)@, and @,@
identWST :: Stream s m Char => ParsecT s u m String
identWST = ident "()," []