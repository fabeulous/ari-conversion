import Test.Problem.Term(termTests)
import Test.HUnit

main :: IO ()
main = do
        putStrLn "Testing terms"
        _ <- runTestTT termTests
        putStrLn "Testing complete."


