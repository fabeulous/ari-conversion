module Test.Problem.Term (termTests) where

import Text.Parsec hiding (parse)
import Data.Conversion.Problem.Term (Term (..), parseTerm)
import Test.HUnit
import Data.Either (isLeft)

termTests :: Test
termTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

-- | Parse a term from a string input where @xs@ is a list of variables
fromString :: [String] -> String -> Either ParseError (Term String String)
fromString xs = runP (parseTerm xs) () ""

test1 :: Test
test1 = TestCase (assertEqual 
    "Parses f(x)"
    (Right (Fun "f" [Var "x"])) 
    (fromString ["x"] "f(x)")
    )

test2 :: Test
test2 = TestCase (assertEqual 
    "Parses a constant"
    (Right (Fun "a" [])) 
    (fromString ["x", "y", "z"] "a")
    )

test3 :: Test
test3 = TestCase (assertEqual 
    "Parses a variable"
    (Right (Var "y")) 
    (fromString ["x", "y", "z"] "y")
    )

test4 :: Test
test4 = TestCase (assertEqual 
    "Parses functions of multiple arities f(x, g(y))"
    (Right (Fun "f" [Var "x", Fun "g" [Var "y"]])) 
    (fromString ["x", "y"] "f(x, g(y))")
    )

test5 :: Test
test5 = TestCase (assertBool 
    "Fails if no parentheses given e.g. f x"
    (isLeft $ fromString ["x", "y"] "f x")
    )

test6 :: Test
test6 = TestCase (assertBool 
    "Fails if too many right parentheses, i.e. f(x))"
    (isLeft $ fromString ["x", "y"] "f(x))")
    )

test7 :: Test
test7 = TestCase (assertBool 
    "Fails if too many left parentheses, i.e. f((x)"
    (isLeft $ fromString ["x", "y"] "f((x)")
    )

test8 :: Test
test8 = TestCase (assertEqual 
    "Works if there are numbers in function names"
    (Right (Fun "f1" [Var "x", Fun "f2" [Var "y"]]))
    (fromString ["x", "y"] "f1(x, f2(y))")
    )

test9 :: Test
test9 = TestCase (assertEqual 
    "Works if there are numbers in variable names"
    (Right (Fun "g" [Var "x4"]))
    (fromString ["x4", "x"] "g(x4)")
    )

test10 :: Test
test10 = TestCase (assertEqual 
    "Works if no variables are given"
    (Right (Fun "f" [Fun "x" [], Fun "y" []]))
    (fromString [] "f(x,y)")
    )