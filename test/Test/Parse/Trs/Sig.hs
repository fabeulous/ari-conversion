module Test.Parse.Trs.Sig (sigTests) where -- qqjf module description

import Data.Conversion.Parser.Parse.Problem.Sig (parseSig)
import Data.Conversion.Problem.Trs.Sig (Sig (..), checkConsistentSig)
import Data.Either (isLeft, isRight)
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

sigTests :: Test
sigTests = TestList [parseSigTests, badSigTests, checkConsistentSigs, checkInconsistentSigs]

-- | qqjf
parseSigTests :: Test
parseSigTests = assertParseList validSigs parseSig
  where
    validSigs :: [(String, [Sig String])]
    validSigs =
      [ ("(f 1)", [Sig "f" 1]),
        ("(f 1) ", [Sig "f" 1]),
        (" (f 1)", [Sig "f" 1]),
        ("  ( f 1  )", [Sig "f" 1]),
        ("(f 2) (a 0) (h 1)", [Sig "f" 2, Sig "a" 0, Sig "h" 1]),
        ("(f 2) (f 1)", [Sig "f" 2, Sig "f" 1]),
        ("(g 25) (g 25)", [Sig "g" 25, Sig "g" 25])
      ]

-- | qqjf
badSigTests :: Test
badSigTests = assertFailParseList badSigs parseSig
  where
    badSigs :: [String]
    badSigs =
      [ "f 1",
        "f 1 g 2",
        "(f 1 g 2)",
        "h -1",
        "(g (1))",
        "(f 1 2)",
        "(f 1) (h 1"
      ]

-- | qqjf
checkConsistentSigs :: Test
checkConsistentSigs =
  TestList
    [ TestCase (assertBool (show sig ++ " is a valid signature. Got " ++ show res) (isRight res))
      | sig <- validSigs,
        let res = checkConsistentSig sig
    ]
  where
    validSigs :: [[Sig String]]
    validSigs =
      [ [Sig "f" 1],
        [Sig "f" 2, Sig "a" 0, Sig "h" 1],
        []
      ]

-- | qqjf assertBool (xs ++ " should not be parseable. Got: " ++ show res) (isLeft res)
checkInconsistentSigs :: Test
checkInconsistentSigs = TestList [TestCase $ assertBadSig sig (checkConsistentSig sig) | sig <- badSigs]
  where
    assertBadSig :: [Sig String] -> Either String [Sig String] -> Assertion
    assertBadSig expected res = assertBool (show expected ++ " is not a valid signature. Got: " ++ show res) (isLeft res)
    badSigs :: [[Sig String]]
    badSigs =
      [ [Sig "f" 1, Sig "f" 2],
        [Sig "f" 1, Sig "f" 1],
        [Sig "f" 1, Sig "a" 0, Sig "f" 1, Sig "f" 2]
      ]
