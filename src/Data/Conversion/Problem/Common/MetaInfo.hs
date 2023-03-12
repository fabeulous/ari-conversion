-- |
-- Module      : Data.Conversion.Problem.Common.MetaInfo
-- Description : MetaInfo type definition
--
-- This module contains an extensible 'MetaInfo' type definition.
module Data.Conversion.Problem.Common.MetaInfo
  ( -- * Types
    MetaInfo (..),

    -- * Defaults
    emptyMetaInfo,
  )
where

import Prettyprinter (Pretty, pretty, tupled, vsep, (<+>))

-- | The type for additional information about a TRS. Includes keywords for the problem origin,
-- doi, and arbitrary comments.
--
-- As all fields are optional, if no value is given then this is represented by @Nothing@:
-- an empty entry is represented by @Just ""@ or @Just []@.
data MetaInfo = MetaInfo
  { -- | Arbitrary comment that does not fit into the remaining 'MetaInfo' categories. e.g. @Just "An example TRS"@
    comment :: Maybe String,
    -- | The doi of the problem if available. e.g. @"10.1007/11805618_6"@
    doi :: Maybe String,
    -- | The origin of the problem. e.g. @"COPS #20"@
    origin :: Maybe String,
    -- | The individual(s) who submitted the problem. e.g. @["Takahito Aoto","Junichi Yoshida","Yoshihito Toyama"]@
    submitted :: Maybe [String]
  }
  deriving (Eq, Show)

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

-- | Allow pretty printing 'MetaInfo's
instance Pretty MetaInfo where
  pretty (MetaInfo cs ds orig sub) =
    vsep
      [ pretty "Comments:  " <+> fromMaybeTuple cs,
        pretty "Doi:       " <+> pretty ds,
        pretty "Origin:    " <+> pretty orig,
        pretty "Submitted: " <+> fromMaybeTuple sub
      ]
    where
      -- Print xs as a comma-separated tuple
      fromMaybeTuple maybeXs = case maybeXs of
        Just xs -> tupled (map pretty xs)
        Nothing -> pretty "No value given"
