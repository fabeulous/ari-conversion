-- |
-- Module      : TRSConversion.Problem.Common.MetaInfo
-- Description : MetaInfo type definition
--
-- This module contains an extensible 'MetaInfo' type definition. It is intended to store
-- any additional information about rewriting systems such as general comments, the originm etc.
module TRSConversion.Problem.Common.MetaInfo
  ( -- * Types
    MetaInfo (..),
    mergeMetaInfo,

    -- * Defaults
    emptyMetaInfo,
  )
where

import Control.Applicative ((<|>))

-- | The type for additional information about a TRS. Includes keywords for the problem 'origin',
-- 'doi', and arbitrary 'comment's.
--
-- All fields are optional:
--
--      * if no value is known then this is represented by @Nothing@.
--      * a known value @xs@ is represented by @Just xs@. In particular, an empty entry is represented by @Just ""@ or @Just []@.
data MetaInfo = MetaInfo
  { -- | Arbitrary comment that does not fit into the remaining 'MetaInfo' categories. e.g. @Just "An example TRS"@
    comment :: Maybe [String],
    -- | The doi of the problem if available. e.g. @Just "10.1007/11805618_6"@
    doi :: Maybe String,
    -- | The origin of the problem. e.g. @Just "COPS #20"@
    origin :: Maybe String,
    -- | The individual(s) who submitted the problem. e.g. @Just ["Takahito Aoto","Junichi Yoshida","Yoshihito Toyama"]@
    submitted :: Maybe [String]
  }
  deriving (Eq, Show)

-- | Merge two @MetaInfo@ values.
--
-- Comments are concatenated, while any doi origin or submitted fields are chosen left biased.
mergeMetaInfo :: MetaInfo -> MetaInfo -> MetaInfo
mergeMetaInfo m1 m2 =
  MetaInfo { doi = doi m1 <|> doi m2
           , origin = origin m1 <|> origin m2
           , submitted = case (submitted m1, submitted m2) of
               (Just a1, Just a2) -> Just (a1 ++ a2)
               (a,b) -> a <|> b
           , comment = case (comment m1, comment m2) of
               (Just c1, Just c2) -> Just (c1 ++ c2)
               (a, b)->  a <|> b
           }

-- | Default value for an empty 'MetaInfo' object. Can be modified as shown below.
--
-- >>> newMetaInfo = emptyMetaInfo { comment = Just "An updated value" }
emptyMetaInfo :: MetaInfo
emptyMetaInfo =
  MetaInfo
    { comment = Nothing,
      doi = Nothing,
      origin = Nothing,
      submitted = Nothing
    }
