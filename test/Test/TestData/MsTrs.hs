-- |
-- Module      : Test.TestData.MsTrs
-- Description : Example data for MsTrs tests
--
-- This module defines test data which is used for testing both parsing and unparsing functions for 'MsTrs's.
-- Exported values can then be imported in 'Test.Parse' and 'Test.Unparse'.
module Test.TestData.MsTrs
  ( -- * Test data for tests on 'MsTrs'
    copsMsTrss,
    ariMsTrss,
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.MsTrs.MsSig (MsSig (..))
import Data.Conversion.Problem.MsTrs.MsTrs (MsTrs (..))

------------------------
--- MSTRS Lists --------
------------------------

-- | A list of MSTRSs in internal 'MsTrs' format and in (COPS format)[http://project-coco.uibk.ac.at/problems/mstrs.php]
-- to test both parsing and unparsing functions.
-- Has format @(original mstrs, str := result of parsing mstrs, result of unparsing str)@.
--
-- The result for unparsing might differ from the original MSTRS did not specify sorts.
copsMsTrss :: [(String, MsTrs String String String, String, MsTrs String String String)]
copsMsTrss =
  [ ( "empty COPS MSTRS",
      emptyMsTrs,
      "(SIG \n)\n(RULES )",
      emptyMsTrs
    ),
    ( "COPS MSTRS with a comment",
      mstrsWithComment,
      "(SIG \n  (0 -> Nat)\n)\n(RULES )\n(COMMENT \nAn MSTRS with a comment)",
      mstrsWithComment {sorts = Nothing}
    ),
    ( "example ground COPS MSTRS",
      groundMsTrs,
      "(SIG \n)\n(RULES \n  a -> b\n)\n(COMMENT \n)",
      groundMsTrs
    ),
    ( "example COPS MSTRS without sorts",
      mstrsWithoutSorts,
      "(SIG \n\
      \  (app List List -> List)\n\
      \  (cons Nat List -> List)\n\
      \  (nil -> List)\n\
      \  (s Nat -> Nat)\n\
      \  (0 -> Nat)\n\
      \)\n\
      \(RULES \n\
      \  app(nil,ys) -> ys\n\
      \  app(cons(x,xs),ys) -> cons(x,app(xs,ys))\n\
      \)",
      mstrsWithoutSorts
    ),
    ( "example COPS MSTRS with unused sorts",
      additionMsTrs,
      "(SIG \n\
      \  (+ Nat Nat -> Nat)\n\
      \  (a -> Nat)\n\
      \  (c -> Nat)\n\
      \  (b -> Nat)\n\
      \)\n\
      \(RULES \n\
      \  +(a,b) -> c\n\
      \)",
      additionMsTrs {sorts = Nothing}
    ),
    ( "COPS MSTRS for COPS #637",
      cops637,
      "(SIG \n\
      \  (+ Nat Nat -> Nat)\n\
      \  (s Nat -> Nat)\n\
      \  (0 -> Nat)\n\
      \  (node Nat Tree Tree -> Tree)\n\
      \  (leaf Nat -> Tree)\n\
      \  (sum Tree -> Nat)\n\
      \)\n\
      \(RULES \n\
      \  sum(leaf(x)) -> x\n\
      \  sum(node(x,yt,zt)) -> +(+(x,sum(yt)),sum(zt))\n\
      \  +(0,y) -> y\n\
      \  +(s(x),y) -> s(+(x,y))\n\
      \  +(x,y) -> +(y,x)\n\
      \  node(x,yt,zt) -> node(x,zt,yt)\n\
      \)\n\
      \(COMMENT \n\
      \experiments for [125]\n\
      \origin: COPS #637\n\
      \submitted by: Takahito Aoto\
      \)",
      cops637
        { sorts = Nothing,
          metaInfo =
            MetaInfo
              { comment = Just "experiments for [125]\norigin: COPS #637\nsubmitted by: Takahito Aoto",
                doi = Nothing,
                origin = Nothing,
                submitted = Nothing
              }
        }
    )
  ]

-- | A list of MSTRSs in internal 'MsTrs' format and in (ARI format)[https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt]
-- to test both parsing and unparsing functions.
-- Has format @(original mstrs, str := result of parsing mstrs, result of unparsing str)@.
--
-- The result for unparsing might differ from the original MSTRS did not specify sorts.
ariMsTrss :: [(String, MsTrs String String String, String, MsTrs String String String)]
ariMsTrss =
  [ ( "empty ARI MSTRS",
      emptyMsTrs,
      "(format MSTRS)",
      emptyMsTrs {sorts = Just []}
    ),
    ( "ARI MSTRS with a comment",
      mstrsWithComment,
      "(meta-info (comment \"An MSTRS with a comment\"))\n(format MSTRS)\n(sort Nat)\n(fun 0 :sort (Nat))",
      mstrsWithComment
    ),
    ( "example ground ARI MSTRS",
      groundMsTrs,
      "(meta-info (comment \"\"))\n\
      \(format MSTRS)\n\
      \(rule a b)", -- Interpreted as variables in ARI format
      groundMsTrs {sorts = Just []}
    ),
    ( "example ARI MSTRS without sorts",
      mstrsWithoutSorts,
      "(format MSTRS)\n\
      \(sort List)\n\
      \(sort Nat)\n\
      \(fun app :sort (List List List))\n\
      \(fun cons :sort (Nat List List))\n\
      \(fun nil :sort (List))\n\
      \(fun s :sort (Nat Nat))\n\
      \(fun 0 :sort (Nat))\n\
      \(rule (app nil ys) ys)\n\
      \(rule (app (cons x xs) ys) (cons x (app xs ys)))",
      mstrsWithoutSorts {sorts = Just ["List", "Nat"]}
    ),
    ( "example ARI MSTRS with unused sorts",
      additionMsTrs,
      "(format MSTRS)\n(sort List)\n(sort Nat)\n(fun + :sort (Nat Nat Nat))\n(fun a :sort (Nat))\n(fun c :sort (Nat))\n(fun b :sort (Nat))\n(rule (+ a b) c)",
      additionMsTrs
    ),
    ( "ARI MSTRS for COPS problem #637",
      cops637,
      "(meta-info (origin \"COPS #637\"))\n\
      \(meta-info (comment \"experiments for [125]\"))\n\
      \(meta-info (submitted \"Takahito Aoto\"))\n\
      \(format MSTRS)\n\
      \(sort Nat)\n\
      \(sort Tree)\n\
      \(fun + :sort (Nat Nat Nat))\n\
      \(fun s :sort (Nat Nat))\n\
      \(fun 0 :sort (Nat))\n\
      \(fun node :sort (Nat Tree Tree Tree))\n\
      \(fun leaf :sort (Nat Tree))\n\
      \(fun sum :sort (Tree Nat))\n\
      \(rule (sum (leaf x)) x)\n\
      \(rule (sum (node x yt zt)) (+ (+ x (sum yt)) (sum zt)))\n\
      \(rule (+ 0 y) y)\n\
      \(rule (+ (s x) y) (s (+ x y)))\n\
      \(rule (+ x y) (+ y x))\n\
      \(rule (node x yt zt) (node x zt yt))",
      cops637
    )
  ]

------------------------
--- MSTRSs -------------
------------------------
emptyMsTrs :: MsTrs String String String
emptyMsTrs = MsTrs {rules = [], signature = [], sorts = Nothing, metaInfo = emptyMetaInfo}

-- Example MSTRS with sorts set but no rules
mstrsWithComment :: MsTrs String String String
mstrsWithComment =
  MsTrs
    { rules = [],
      signature = [zeroSig],
      sorts = Just ["Nat"],
      metaInfo = emptyMetaInfo {comment = Just "An MSTRS with a comment"}
    }

groundMsTrs :: MsTrs String String String
groundMsTrs =
  MsTrs
    { rules = [Rule {lhs = Var "a", rhs = Var "b"}],
      signature = [],
      sorts = Nothing,
      metaInfo = emptyMetaInfo {comment = Just ""}
    }

-- | Example MSTRS without sorts specified to test 'inferSorts'
mstrsWithoutSorts :: MsTrs String String String
mstrsWithoutSorts =
  MsTrs
    { rules =
        [ Rule {lhs = Fun "app" [Fun "nil" [], Var "ys"], rhs = Var "ys"},
          Rule {lhs = Fun "app" [Fun "cons" [Var "x", Var "xs"], Var "ys"], rhs = Fun "cons" [Var "x", Fun "app" [Var "xs", Var "ys"]]}
        ],
      signature =
        [ MsSig "app" (["List", "List"], "List"),
          MsSig "cons" (["Nat", "List"], "List"),
          MsSig "nil" ([], "List"),
          MsSig "s" (["Nat"], "Nat"),
          MsSig "0" ([], "Nat")
        ],
      sorts = Nothing,
      metaInfo = emptyMetaInfo
    }

-- | An example MSTRS with an unused sort @"List"@
additionMsTrs :: MsTrs String String String
additionMsTrs =
  MsTrs
    { rules = [additionRule],
      signature = additionSig,
      sorts = Just ["List", "Nat"],
      metaInfo = emptyMetaInfo
    }

-- | An example MSTRS based on COPS problem #637
cops637 :: MsTrs String String String
cops637 =
  MsTrs
    { rules =
        [ Rule {lhs = Fun "sum" [Fun "leaf" [Var "x"]], rhs = Var "x"},
          Rule {lhs = Fun "sum" [Fun "node" [Var "x", Var "yt", Var "zt"]], rhs = Fun "+" [Fun "+" [Var "x", Fun "sum" [Var "yt"]], Fun "sum" [Var "zt"]]},
          Rule {lhs = Fun "+" [Fun "0" [], Var "y"], rhs = Var "y"},
          Rule {lhs = Fun "+" [Fun "s" [Var "x"], Var "y"], rhs = Fun "s" [Fun "+" [Var "x", Var "y"]]},
          Rule {lhs = Fun "+" [Var "x", Var "y"], rhs = Fun "+" [Var "y", Var "x"]},
          Rule {lhs = Fun "node" [Var "x", Var "yt", Var "zt"], rhs = Fun "node" [Var "x", Var "zt", Var "yt"]}
        ],
      signature =
        [ MsSig "+" (["Nat", "Nat"], "Nat"),
          MsSig "s" (["Nat"], "Nat"),
          MsSig "0" ([], "Nat"),
          MsSig "node" (["Nat", "Tree", "Tree"], "Tree"),
          MsSig "leaf" (["Nat"], "Tree"),
          MsSig "sum" (["Tree"], "Nat")
        ],
      sorts = Just ["Nat", "Tree"],
      metaInfo =
        emptyMetaInfo
          { comment = Just "experiments for [125]",
            doi = Nothing,
            origin = Just "COPS #637",
            submitted = Just ["Takahito Aoto"]
          }
    }

------------------------
--- Rules and sigs -----
------------------------
zeroSig :: MsSig String String
zeroSig = MsSig "0" ([], "Nat")

additionSig :: [MsSig String String]
additionSig = [MsSig "+" (["Nat", "Nat"], "Nat"), MsSig "a" ([], "Nat"), MsSig "c" ([], "Nat"), MsSig "b" ([], "Nat")]

additionRule :: Rule String String
additionRule = Rule {lhs = Fun "+" [Fun "a" [], Fun "b" []], rhs = Fun "c" []}