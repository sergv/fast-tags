-- |
-- Module:     FastTags.CompactFormat
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ImportQualifiedPost #-}

module FastTags.CompactFormat
    ( writeTo
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import System.IO (Handle)

import FastTags.Tag qualified as Tag
import FastTags.Token qualified as Token
import FastTags.Util qualified as Util

import System.OsPath

writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> [Text]
writeTo = undefined
    -- = map (uncurry formatFileTags)
    -- . map (fmap (dropAdjacent maxSeparation))
    -- . Util.groupOnKey (Token.posFile . Token.posOf)

