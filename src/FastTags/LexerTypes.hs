{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module FastTags.LexerTypes where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)

import Control.Monad.EitherK

import FastTags.Token
import qualified FastTags.Util as Util

{-# INLINE advanceLine #-}
advanceLine :: Char -> Line -> Line
advanceLine '\n' = increaseLine
advanceLine _    = id

countInputSpace :: AlexInput -> Int -> Int
countInputSpace input len =
    countSpace $ Text.take len $ aiInput input
    where
    countSpace :: Text -> Int
    countSpace = Text.foldl' inc 0
        where
        inc acc ' '    = acc + 1
        inc acc '\t'   = acc + 8
        inc acc '\x01' = acc + 1
        inc acc _      = acc


data AlexInput = AlexInput {
    aiInput           :: Text
    , aiPrevChar      :: {-# UNPACK #-} !Char
    , aiBytes         :: [Word8]
    , aiLine          :: {-# UNPACK #-} !Line
    , aiTrackPrefixes :: Bool
    , aiPrefix        :: Text
    , aiAbsPos        :: {-# UNPACK #-} !Int
    } deriving (Show, Eq, Ord)

mkAlexInput :: Text -> Bool -> AlexInput
mkAlexInput s trackPrefixes = AlexInput
    { aiInput         = s'
    , aiPrevChar      = '\n'
    , aiBytes         = []
    , aiLine          = initLine
    , aiTrackPrefixes = trackPrefixes
    , aiPrefix        = Text.empty
    , aiAbsPos        = initAbsPos
    }
    where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0
    -- Same reasoning applies to the initial absolute position.
    initAbsPos = -1

    s' = Text.cons '\n' $ Text.snoc (stripBOM s) '\n'
    stripBOM :: Text -> Text
    stripBOM xs =
        fromMaybe xs $
        Text.stripPrefix utf8BOM xs <|> Text.stripPrefix utf8BOM' xs
    -- utf8BOM = "\xEF\xBB\xBF"
    utf8BOM = "\xFFEF"
    utf8BOM' = "\xFEFF"

mkSrcPos :: FilePath -> AlexInput -> SrcPos
mkSrcPos filename (AlexInput {aiInput, aiLine, aiPrefix, aiAbsPos}) =
    SrcPos { posFile = filename
           , posLine = aiLine
           , posOffset = Offset aiAbsPos
           , posPrefix = aiPrefix
           , posSuffix = Text.takeWhile (/= '\n') aiInput
           }

-- TODO: Not very efficient to snoc every character here, figure out something
-- better.
updatePrefix :: Char -> AlexInput -> AlexInput
updatePrefix c input@(AlexInput {aiTrackPrefixes, aiPrefix})
    | aiTrackPrefixes = input
        { aiPrefix = case c of
            '\n' -> Text.empty
            _    -> Text.snoc aiPrefix c
        }
    | otherwise       = input

data Context = CtxHaskell | CtxQuasiquoter
    deriving (Show, Eq, Ord)

data AlexState = AlexState {
    asInput                       :: AlexInput
 -- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
 -- or vanilla toplevel mode.
    , asCode                      :: {-# UNPACK #-} !Int
    , asCommentDepth              :: {-# UNPACK #-} !Int
    , asQuasiquoterDepth          :: {-# UNPACK #-} !Int
    , asIndentationSize           :: {-# UNPACK #-} !Int
    , asContextStack              :: [Context]
    , asPositionsOfQuasiQuoteEnds :: Maybe IntSet
    } deriving (Show, Eq, Ord)

mkAlexState :: AlexInput -> AlexState
mkAlexState input = AlexState
    { asInput                     = input
    , asCode                      = 0
    , asCommentDepth              = 0
    , asQuasiquoterDepth          = 0
    , asIndentationSize           = 0
    , asContextStack              = []
    , asPositionsOfQuasiQuoteEnds = Nothing
    }

{-# INLINE pushContext #-}
pushContext :: (MonadState AlexState m) => Context -> m ()
pushContext ctx = modify (\s -> s { asContextStack = ctx : asContextStack s })

{-# INLINE modifyCommentDepth #-}
modifyCommentDepth :: (MonadState AlexState m) => (Int -> Int) -> m Int
modifyCommentDepth f = do
    depth <- gets asCommentDepth
    let depth' = f depth
    modify $ \s -> s { asCommentDepth = depth' }
    return depth'

{-# INLINE modifyQuasiquoterDepth #-}
modifyQuasiquoterDepth :: (MonadState AlexState m) => (Int -> Int) -> m Int
modifyQuasiquoterDepth f = do
    depth <- gets asQuasiquoterDepth
    let depth' = f depth
    modify $ \s -> s { asQuasiquoterDepth = depth' }
    return depth'

retrieveToken :: AlexInput -> Int -> Text
retrieveToken (AlexInput {aiInput}) len = Text.take len aiInput

{-# INLINE addIndentationSize #-}
addIndentationSize :: (MonadState AlexState m) => Int -> m ()
addIndentationSize x =
    modify (\s -> s { asIndentationSize = x + asIndentationSize s })

data QQEndsState = QQEndsState
    { qqessPos      :: {-# UNPACK #-} !Int
    , qqessMap      :: !IntSet
    , qqessPrevChar :: {-# UNPACK #-} !Char
    }

calculateQuasiQuoteEnds :: Int -> Text -> IntSet
calculateQuasiQuoteEnds startPos =
    qqessMap . Text.foldl' combine (QQEndsState startPos IntSet.empty '\n')
    where
    combine :: QQEndsState -> Char -> QQEndsState
    combine QQEndsState{qqessPos, qqessMap, qqessPrevChar} c = QQEndsState
        { qqessPos      = qqessPos + 1
        , qqessMap      =
              case (qqessPrevChar, c) of
                  ('|', ']') -> IntSet.insert qqessPos qqessMap
                  (_,   '⟧') -> IntSet.insert qqessPos qqessMap
                  _          -> qqessMap
        , qqessPrevChar = c
        }

type AlexM = EitherKT String (State AlexState)

runAlexM :: Bool -> Text -> AlexM a -> Either String a
runAlexM trackPrefixes input action =
    evalState (runEitherKT action (return . Left) (return . Right)) s
    where
    s = mkAlexState $ mkAlexInput input trackPrefixes

{-# INLINE alexSetInput #-}
alexSetInput :: (MonadState AlexState m) => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

{-# INLINE alexSetStartCode #-}
alexSetStartCode :: (MonadState AlexState m) => Int -> m ()
alexSetStartCode code = modify $ \s -> s { asCode = code }

-- Alex interface
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@(AlexInput {aiInput, aiBytes, aiLine, aiAbsPos}) =
    case aiBytes of
        b:bs -> Just (b, input { aiBytes = bs })
        []   -> nextChar
    where
    nextChar = case Text.uncons aiInput of
        Nothing      -> Nothing
        Just (c, cs) -> encode (fromMaybe c $ fixChar c) cs
    encode c cs =
        case encodeChar c of
            b:bs -> Just (b, updatePrefix c input')
                where
                input' = input { aiInput    = cs
                               , aiBytes    = bs
                               , aiPrevChar = c
                               , aiLine     = advanceLine c aiLine
                               , aiAbsPos   = aiAbsPos + 1
                               }
            []   -> emptyUtfEncodingError
    emptyUtfEncodingError = error
        "alexGetByte: should not happen - utf8 encoding of a character is empty"

-- Translate unicode character into special symbol we teached Alex to recognize.
fixChar :: Char -> Maybe Char
-- These should not be translated since Alex knows about them
fixChar '→' = Nothing
fixChar '∷' = Nothing
fixChar '⇒' = Nothing
fixChar '∀' = Nothing
fixChar c
    | c <= '\x7f' = Nothing -- Plain ascii needs no fixing.
    | otherwise
    = case generalCategory c of
          UppercaseLetter       -> Just upper
          LowercaseLetter       -> Just lower
          TitlecaseLetter       -> Just upper
          ModifierLetter        -> Just suffix
          OtherLetter           -> Just lower
          DecimalNumber         -> Just digit
          OtherNumber           -> Just digit
          Space                 -> Just space
          other                 ->
              if Util.isSymbolCharacterCategory other
              then Just symbol
              else Nothing
    where
    space  = '\x01'
    upper  = '\x02'
    lower  = '\x03'
    symbol = '\x04'
    digit  = '\x05'
    suffix = '\x06'
