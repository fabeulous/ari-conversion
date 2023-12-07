{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : TRSConversion.Parse.Problem.MetaInfo
Description : Comment parser

This module defines parsers to parse the additional information (comment, author, etc.) of a
given rewriting system.
-}
module TRSConversion.Formats.ARI.Parse.MetaInfo (
    -- * ARI
    parseAriMetaInfo,
)
where

import Data.Text (Text, unpack)
import Text.Megaparsec (MonadParsec (notFollowedBy, withRecovery), between, eof, many, registerParseError, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (char, eol, space, string)

import Control.Monad (void)
import Data.Foldable (foldl')
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, isNewline)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo, mergeMetaInfo)

{- | Parse meta-info blocks in ARI format

If multiple DOIs are specified only the first is used.

Expects e.g. something like
@
; @origin COPS #20
; @doi 10.1007/11805618_6
; @author Takahito Aoto
; @author Junichi Yoshida
; @author Yoshihito Toyama
; [7] Example 2
@.
-}
parseAriMetaInfo :: ARIParser MetaInfo
parseAriMetaInfo =
    foldl' mergeMetaInfo emptyMetaInfo
        <$> many (structuredMeta <|> ariLeadingComment)

structuredMeta :: ARIParser MetaInfo
structuredMeta = structure $ ariAuthorLine <|> ariDoiLine <|> ariOriginLine <|> ariCopsLine
  where
    structure =
        between (try (string "; @")) ((void eol <|> eof) <* space) . continue

    continue = withRecovery $ \err -> do
        registerParseError err
        _ <- takeWhileP Nothing (not . isNewline)
        pure emptyMetaInfo

metaKeyValue :: Text -> ARIParser Text
metaKeyValue key = do
    _ <- string key <* char ' '
    takeWhileP (Just "character") (not . isNewline)

ariDoiLine :: ARIParser MetaInfo
ariDoiLine = do
    doiStr <- metaKeyValue "doi"
    pure $ emptyMetaInfo{doi = Just $ unpack doiStr}

ariOriginLine :: ARIParser MetaInfo
ariOriginLine = do
    originStr <- metaKeyValue "origin"
    pure $ emptyMetaInfo{origin = Just $ unpack originStr}

ariCopsLine :: ARIParser MetaInfo
ariCopsLine = do
    originStr <- metaKeyValue "cops"
    pure $ emptyMetaInfo{copsNum = Just $ unpack originStr}

ariAuthorLine :: ARIParser MetaInfo
ariAuthorLine = do
    doiStr <- metaKeyValue "author"
    pure $ emptyMetaInfo{submitted = Just [unpack doiStr]}

ariLeadingComment :: ARIParser MetaInfo
ariLeadingComment = between commentStart eol $ do
    cmt <- takeWhileP (Just "character") (not . isNewline)
    pure $ emptyMetaInfo{comment = Just [unpack cmt]}
  where
    commentStart = try (char ';' <* notFollowedBy (string " @"))
