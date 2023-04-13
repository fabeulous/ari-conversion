{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : TRSConversion.Parse.Problem.MetaInfo
Description : Comment parser

This module defines parsers to parse the additional information (comment, author, etc.) of a
given rewriting system.
-}
module TRSConversion.Parse.ARI.MetaInfo (
    -- * ARI
    parseAriMetaInfo,
)
where

import Data.Text (Text, unpack)
import Text.Megaparsec (MonadParsec (notFollowedBy, withRecovery), between, eof, many, registerParseError, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (char, string)

import Control.Monad (void)
import Data.Foldable (foldl')
import TRSConversion.Parse.Utils (Parser)
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
parseAriMetaInfo :: Parser MetaInfo
parseAriMetaInfo = do
    meta <- foldl' mergeMetaInfo emptyMetaInfo <$> many structuredMeta
    comments <- foldl' mergeMetaInfo emptyMetaInfo <$> many ariLeadingComment
    pure $ mergeMetaInfo meta comments

structuredMeta :: Parser MetaInfo
structuredMeta = structure $ ariAuthorLine <|> ariDoiLine
  where
    structure = between (try (string "; @")) (void (char '\n') <|> eof) . continue

    continue = withRecovery $ \err -> do
        registerParseError err
        _ <- takeWhileP Nothing (/= '\n')
        pure emptyMetaInfo

metaKeyValue :: Text -> Parser Text
metaKeyValue key = do
    _ <- string key <* char ' '
    takeWhileP (Just "character") (\c -> c `notElem` ['\r', '\n'])

ariDoiLine :: Parser MetaInfo
ariDoiLine = do
    doiStr <- metaKeyValue "doi"
    pure $ emptyMetaInfo{doi = Just $ unpack doiStr}

ariAuthorLine :: Parser MetaInfo
ariAuthorLine = do
    doiStr <- metaKeyValue "author"
    pure $ emptyMetaInfo{submitted = Just [unpack doiStr]}

ariLeadingComment :: Parser MetaInfo
ariLeadingComment = between commentStart (char '\n') $ do
    cmt <- takeWhileP (Just "character") (/= '\n')
    pure $ emptyMetaInfo{comment = Just [unpack cmt]}
  where
    commentStart = try (char ';' <* notFollowedBy (string " @"))
