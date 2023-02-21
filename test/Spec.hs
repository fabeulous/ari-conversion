import Test.Problem.Term(termTests)
import Test.HUnit

main :: IO ()
main = do
        putStrLn "Testing term parsing"
        _ <- runTestTT termTests
        putStrLn "Testing complete."


