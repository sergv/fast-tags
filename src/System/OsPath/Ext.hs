-- |
-- Module:     System.OsPath.Ext
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ImportQualifiedPost #-}

module System.OsPath.Ext
  ( textToShortByteString
  , textFromShortByteString
  , stripProperPrefix
  , pathToText
  , pathFromUtf8BS
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Coerce
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as T
import System.OsPath
import System.OsString.Internal.Types

pathToUtf8 :: OsPath -> ShortByteString
pathToUtf8 =
#ifdef mingw32_HOST_OS
  _reconvertUtf16LEToUtf8 . getWindowsString . getOsString
#else
  getPosixString . getOsString
#endif

pathToText :: OsPath -> Text
pathToText = textFromShortByteString . pathToUtf8

pathFromUtf8BS :: ByteString -> OsPath
pathFromUtf8BS = pathFromUtf8 . BSS.toShort

pathFromUtf8 :: ShortByteString -> OsPath
pathFromUtf8 =
#ifdef mingw32_HOST_OS
  OsString . WindowsString . _reconvertUtf8ToUtf16LE
#else
  OsString . PosixString
#endif

_reconvertUtf16LEToUtf8 :: ShortByteString -> ShortByteString
_reconvertUtf16LEToUtf8 = textToShortByteString . TE.decodeUtf16LE . BSS.fromShort

_reconvertUtf8ToUtf16LE :: ShortByteString -> ShortByteString
_reconvertUtf8ToUtf16LE = BSS.toShort . TE.encodeUtf16LE . textFromShortByteString

textToShortByteString :: Text -> ShortByteString
textToShortByteString (T.Text (TA.ByteArray arr) _offset _len) = BSS.SBS arr

textFromShortByteString :: ShortByteString -> Text
textFromShortByteString str@(BSS.SBS arr) = T.Text (TA.ByteArray arr) 0 (BSS.length str)

stripProperPrefix :: OsPath -> OsPath -> Maybe OsPath
stripProperPrefix = coerce BSS.stripPrefix

