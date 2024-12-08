{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


{-# LANGUAGE QuasiQuotes #-}

{- | Tagify haskell source.

    The key features are to be fast, incremental (i.e. merge tags with one
    file into existing tags), work on partially edited code, and work with
    hsc. That way I can hook it to the editor's save action and always keep
    the tags up to date.
-}
module FastTags.Main (main) where
#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative
#endif
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import Control.Monad

import Control.Concurrent (myThreadId, ThreadId, getNumCapabilities)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TVar
import Control.Exception (bracket, finally, SomeException)
import Data.Foldable
import Data.Traversable

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Version as Version

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified FastTags.Emacs as Emacs
import qualified FastTags.Tag as Tag
import qualified FastTags.Token as Token
import qualified FastTags.Util as Util
import qualified FastTags.Vim as Vim

import qualified FastTags.CompactFormat as CompactFormat
import System.OsPath

import qualified Paths_fast_tags


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.OsPath.Ext


options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["compact-format"] (GetOpt.NoArg CompactFormat)
        "generate tags in compact Emacs format"
    , GetOpt.Option ['e'] ["emacs"] (GetOpt.NoArg ETags)
        "generate tags in Emacs format"
    , GetOpt.Option [] ["exclude"] (GetOpt.ReqArg Exclude "pattern") $ concat
        [ "Add a pattern to a list of files to exclude when -R is given."
        , "  The pattern is matched against the basename and complete path of"
        , " each file and directory.  This features is based on exuberant"
        , " ctags."
        ]
    , GetOpt.Option ['h'] ["help"] (GetOpt.NoArg Help)
        "print help message"
    , GetOpt.Option ['L'] ["follow-symlinks"] (GetOpt.NoArg FollowSymlinks)
        "follow symlinks when -R is given"
    , GetOpt.Option [] ["nomerge"] (GetOpt.NoArg NoMerge)
        "replace an existing tags file instead of merging into it"
    , GetOpt.Option [] ["no-module-tags"] (GetOpt.NoArg NoModuleTags)
        "do not generate tags for modules"
    , GetOpt.Option ['o'] [] (GetOpt.ReqArg Output "file")
        "output file, defaults to 'tags'"
    , GetOpt.Option [] ["qualified"] (GetOpt.NoArg Qualified) $ concat
        [ "Each tag gets a version qualified with its module name, like M.f,"
        , " and an unqualified version."
        ]
    , GetOpt.Option [] ["fully-qualified"] (GetOpt.NoArg FullyQualified) $
        concat
        [ "Like --qualified, but the tag is fully qualified, A.B.C.f."
        , " Use with qualified_tag.py."
        ]
    , GetOpt.Option ['v'] ["verbose"] (GetOpt.NoArg Verbose)
        "print files as they are tagged, useful to track down slow files"
    , GetOpt.Option [] ["version"] (GetOpt.NoArg Version)
        "print current version"
    , GetOpt.Option ['0'] [] (GetOpt.NoArg ZeroSep)
        "expect list of file names on stdin to be 0-separated."
    ]

help :: String
help = unlines
    [ "usage: fast-tags [options] [filenames]"
    , ""
    , "If a single '-' is given for filenames, fast-tags expects a list of"
    , "files separated by newlines on stdin."
    , ""
    , "A tag will suppress any other tags with the same name within 2"
    , "lines.  This should prevent multiple tag matches for things like"
    , "`data X = X`.  Currently the 2 line limit is not configurable."
    ]

-- | Suppress tags with the same name within this number of lines.
maxSeparation :: Int
maxSeparation = 2

type Pattern = String

data Flag =
    CompactFormat
    | ETags
    | Exclude !Pattern
    | FollowSymlinks
    | FullyQualified
    | Help
    | NoMerge
    | NoModuleTags
    | Output !FilePath
    | Qualified
    | Verbose
    | Version
    | ZeroSep
    deriving (Eq, Show)

main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, inputs) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, inputs, []) -> (flags, ) <$> traverse encodeUtf inputs
        (_, _, errs) -> usage $ "flag errors:\n" ++ List.intercalate ", " errs

    when (Help `elem` flags) $ usage ""
    when (Version `elem` flags) $ do
        putStrLn $ "fast-tags, version "
            ++ Version.showVersion Paths_fast_tags.version
        Exit.exitSuccess

    let verbose       = Verbose `elem` flags
        emacs         = ETags `elem` flags
        compactFormat = CompactFormat `elem` flags
        vim           = not emacs
        trackPrefixes = emacs
        output        = last $ defaultOutput : [fn | Output fn <- flags]
        defaultOutput = if vim then "tags" else "TAGS"

    oldTags <- if vim && NoMerge `notElem` flags
        then do
            exists <- Directory.doesFileExist output
            if exists
                then Text.lines <$> Util.readFileLenient output
                else return []
        else return [] -- we do not support tags merging for emacs for now

    inputs <- Util.unique . map normalise <$> getInputs flags inputs
    when (null inputs) $
        Exit.exitSuccess

    stderr <- MVar.newMVar IO.stderr
    jobs   <- getNumCapabilities
    (newTags :: [(OsPath, [Token.Pos Tag.TagVal])]) <-
        parMapNondet
            [1..jobs]
            (zip [0 :: Int ..] inputs)
            $ \_ (i, fn) -> do
                Exception.handle (catchError stderr fn) $ do
                    (newTags, warnings) <- Tag.processFile fn trackPrefixes
                    newTags <- return $ if NoModuleTags `elem` flags
                        then filter ((/=Tag.Module) . typeOf) newTags else newTags
                    -- Try to do work before taking the lock.
                    Exception.evaluate $ DeepSeq.rnf warnings
                    MVar.withMVar stderr $ \hdl ->
                        mapM_ (IO.hPutStrLn hdl) warnings
                    when verbose $ do
                        let line = take 78 $ show i ++ ": " ++ show fn
                        putStr $ '\r' : line ++ replicate (78 - length line) ' '
                        IO.hFlush IO.stdout
                    return (fn, newTags)

    when verbose $ putChar '\n'

    let allTags = if vim
            then Vim.merge maxSeparation inputs newTags oldTags
            else Emacs.format maxSeparation newTags
    let write = if vim then Text.IO.hPutStrLn else Text.IO.hPutStr
    let withOutput action = if output == "-"
            then action IO.stdout
            else IO.withFile output IO.WriteMode action
    withOutput $ \hdl -> do
      IO.hSetEncoding hdl IO.utf8
      if compactFormat
      then CompactFormat.writeTo hdl newTags
      else mapM_ (write hdl) allTags

    where
    usage msg = do
        putStr $ GetOpt.usageInfo (msg ++ "\n" ++ help) options
        Exit.exitFailure

catchError :: MVar.MVar IO.Handle -> OsPath -> Exception.SomeException
    -> IO (OsPath, [a])
catchError stderr fn e = do
    MVar.withMVar stderr $ \hdl -> IO.hPutStrLn hdl $
        "Error while analyzing " ++ show fn ++ ":\n" ++ show e
    return (fn, [])

typeOf :: Token.Pos Tag.TagVal -> Tag.Type
typeOf tagVal = case Token.valOf tagVal of
    Tag.TagVal _ typ _ -> typ

-- | Expand file inputs from cmdline.
getInputs :: [Flag] -> [OsPath] -> IO [OsPath]
getInputs flags inputs
    | inputs == [[osp|-|]] = map pathFromUtf8BS . filter (not . C8.null) . C8.split sep <$> BS.getContents
    | otherwise            = pure inputs
    where
    sep = if ZeroSep `elem` flags then '\0' else '\n'

parMapNondet :: forall r a b. [r] -> [a] -> (r -> a -> IO b) -> IO [b]
parMapNondet rs as f = do
    resources      <- newTChanIO
    tasks          <- newTMChanIO
    results        <- newTVarIO []
    tid            <- myThreadId
    bracket
        (for rs' $ \_ -> Async.async $ process tid resources tasks results)
        Async.cancelMany
        $ \handles -> do
            traverse_ (atomically . writeTChan resources) rs
            for_ as $ \a -> atomically $ do
                r <- readTChan resources
                writeTMChan tasks (r, a)
            atomically $ closeTMChan tasks
            traverse_ Async.wait handles
            atomically $ readTVar results
    where
        rs' :: [(Int, r)]
        rs' = zip [0..] rs
        process :: ThreadId -> TChan r -> TMChan (r, a) -> TVar [b] -> IO ()
        process master resources tasks results = go
            where
                go = do
                    res <- atomically $ readTMChan tasks
                    case res of
                        Nothing     -> pure ()
                        Just (r, a) -> do
                            !b <- (f r a `finally` atomically (writeTChan resources r)) `Exception.catch`
                                \(e :: SomeException) -> do
                                    Exception.throwTo master e
                                    Exception.throwIO e
                            atomically $ modifyTVar results $ (b :)
                            go
