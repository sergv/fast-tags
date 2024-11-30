-- |
-- Module:     FastTags.CompactFormat
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FastTags.CompactFormat
    ( writeTo
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLBI
import Data.Text.Unsafe qualified as TU
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable
import GHC.Magic (inline)
import System.IO (Handle, hPutChar)

import FastTags.Tag qualified as Tag
import FastTags.Token qualified as Token

import System.OsPath
import System.OsPath.Ext

writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> IO ()
writeTo dest xs = do
    list $
        for_ xs $ \(fn, tags) -> list $ do
            txt $ pathToText fn
            for_ tags $ \Token.Pos{Token.posOf = Token.SrcPos{Token.posLine}, Token.valOf = Tag.TagVal{Tag.tvName, Tag.tvType, Tag.tvParent}} ->
                list $ do
                    int $ Token.unLine posLine
                    txt tvName
                    case tvParent of
                        Nothing                                    -> do
                            rawchar '.'
                            lispChar $ toType tvType
                            pure ()
                        Just Tag.ParentTag{Tag.ptName, Tag.ptType} -> do
                            lispChar $ toType tvType
                            txt ptName
                            rawchar '.'
                            lispChar $ toType ptType
    where
        list :: IO a -> IO a
        list action = rawchar '(' *> action <* rawchar ')'

        _bstr :: ByteString -> IO ()
        _bstr s = do
            rawchar '"'
            () <- foldBSM qchar s
            rawchar '"'

        txt :: Text -> IO ()
        txt s = do
            rawchar '"'
            textFoldM_ qchar s
            rawchar '"'

        rawchar :: Char -> IO ()
        rawchar = hPutChar dest

        qchar :: Char -> IO ()
        qchar = \case
            '"'  -> C8.hPutStr dest "\\\""
            '\\' -> C8.hPutStr dest "\\\\"
            c    -> rawchar c

        lispChar :: Char -> IO ()
        lispChar c = do
            rawchar '?'
            qchar c

        int :: Int -> IO ()
        int = T.hPutStr dest . TL.toStrict . TLB.toLazyText . TLBI.decimal

foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a
foldBSM f (BSI.BS ptr len) = do
    let ptr' = unsafeForeignPtrToPtr ptr
    let go !acc !n
            | n == len
            = pure acc
            | otherwise
            = do
                b <- liftIO $ peekByteOff ptr' n
                x <- f (BSI.w2c b)
                go (acc <> x) (n + 1)
    res <- go mempty 0
    liftIO $ touchForeignPtr ptr
    pure res

{-# INLINE textFoldM_ #-}
textFoldM_ :: forall m. Monad m => (Char -> m ()) -> Text -> m ()
textFoldM_ f (TI.Text arr off len) =
  textFoldLoop off
  where
    !end = off + len
    textFoldLoop :: Int -> m ()
    textFoldLoop !j
      | j >= end  = pure ()
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        inline f c
        textFoldLoop (j + delta)

toType :: Tag.Type -> Char
toType typ = case typ of
    Tag.Module      -> 'm'
    Tag.Function    -> 'f'
    Tag.Class       -> 'c'
    Tag.Type        -> 't'
    Tag.Constructor -> 'C'
    Tag.Operator    -> 'o'
    Tag.Pattern     -> 'p'
    Tag.Family      -> 'F'
    Tag.Define      -> 'D'
