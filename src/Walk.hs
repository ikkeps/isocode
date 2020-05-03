{-# LANGUAGE TupleSections #-}

module Main where

import Text.Pretty.Simple (pPrint)

import System.Directory
import System.FilePath ((</>))
import qualified Options.Applicative as Opt
import Control.Monad (when)
import Control.Applicative ((<**>), (<|>))
import Data.List
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import Parse (parseFile, Expr)
import Matcher (findMatches, Match(..))
import System.Exit (exitSuccess)
import Transform (transform)
import qualified Data.ByteString.Internal as BI
import System.Console.ANSI as A
import System.Exit (exitWith, ExitCode(..))

data Arguments = Arguments
  { pattern     :: Either FilePath String
  , searchPaths :: [String]
  , verbose     :: Bool
  , justParse   :: Bool
  , fileNamesOnly :: Bool
  , recursive   :: Bool
  , lineNumber  :: Bool
  }

getArguments :: IO Arguments
getArguments = Opt.execParser commandLineParser

argumentsParser :: Opt.Parser Arguments
argumentsParser = Arguments
    <$> (
            (Right <$> Opt.strArgument (Opt.metavar "PATTERN" <> Opt.help "code sample to search for"))
        <|> (Right <$> Opt.strOption (Opt.short 'e' <> Opt.long "regexp" <> Opt.metavar "PATTERN" <> Opt.help "code sample to search for"))
        <|> (Left  <$> Opt.strOption (Opt.short 'f' <> Opt.long "file" <> Opt.metavar "PATTERN_FILE" <> Opt.help "path to code sample to search in"))
        )
    <*> (Opt.some $ Opt.strArgument (Opt.metavar "SEARCH_PATHS" <> Opt.help "files or dirs to search"))
    <*> Opt.switch (Opt.short 'W' <> Opt.long "verbose" <> Opt.help "(non grep compat) 'WHAT?' spew debug information")
    <*> Opt.switch (Opt.short 'Y' <> Opt.long "debug" <> Opt.help "(non grep compat) 'WHY?' just parse the pattern and show, do not search for it")
    <*> Opt.switch (Opt.short 'l' <> Opt.long "files-with-matches" <> Opt.help "Show only filenames of matched files")
    <*> Opt.switch (Opt.short 'r' <> Opt.long "recursive" <> Opt.help "IGNORED, always on")
    <*> Opt.switch (Opt.short 'n' <> Opt.long "line-number" <> Opt.help "IGNORED, always on")

commandLineParser :: Opt.ParserInfo Arguments
commandLineParser = Opt.info (argumentsParser <**> Opt.helper)
   ( Opt.fullDesc
  <> Opt.progDesc "Searches for code similar to PATTERN_FILE or PATTERN in SEARCH_DIR."
  <> Opt.header "isocode - search source tree by Perl code sample" )

perlExtensions :: [String]
perlExtensions = [".pl", ".pm", ".t", ".comp", ".inc", ".html"]

main :: IO ()
main = do

    args <- getArguments
    
    blob <- loadPattern $ pattern args  
    
    when (verbose args || justParse args) $ putStrLn "Parsing...\n"

    exprs <- eitherFail $ parseFile blob

    when (verbose args || justParse args) $ do
        pPrint exprs
        putStrLn ""
        putStrLn "Matching pattern:"
        putStrLn ""

    exprs <- eitherFail $ transform exprs

    when (verbose args || justParse args) $ do
        pPrint exprs
        putStrLn ""

    when (justParse args) $ exitSuccess
    when (verbose args) $ putStrLn "Scanning..."

    allFileNames <- concat <$> mapM (\path -> canonicalizePath path >>= recursiveFiles) (searchPaths args)
    
    let fileNames = filter isPerlFileName allFileNames

    matches <- matchFiles exprs fileNames
    
    let found = mapMaybe anyMatches matches

    if fileNamesOnly args then
        showMatchingFilePaths found
    else do
        mapM_ showFileMatches found 
        showStats allFileNames matches found

    if null found then
        exitWith ExitSuccess
    else
        exitWith $ ExitFailure 1 -- mimic grep

    where
        isPerlFileName p = any (`isSuffixOf` p) perlExtensions
    
        anyMatches :: (FilePath, Either String [Match]) -> Maybe (FilePath, [Match])
        anyMatches (_, (Right [])) = Nothing
        anyMatches (name, (Right matches)) = Just (name, matches)
        anyMatches (_, (Left _)) = Nothing

eitherFail :: Either String a -> IO a
eitherFail e = either crash return e
    where
        crash :: String -> IO a
        crash msg = do
            putStrLn msg
            exitWith $ ExitFailure 2

showStats :: [FilePath] -> [(FilePath, Either String [Match])] -> [(FilePath, [Match])] -> IO ()
showStats allFileNames matches found = do
    let errors = filter isError matches
    mapM_ putStrLn [
          "Files: " ++ show (length allFileNames)
        , "Scanned " ++ show (length matches) ++ " files with " ++ show (length errors) ++ " errors"
        , "Total " ++ show (length found) ++ " files matches"
        , "Total " ++ show (sum $ fmap (length . snd) found) ++ " matches"
        ]
    where
        isError (_, (Left _)) = True
        isError _ = False

matchFiles :: [Expr] -> [FilePath] -> IO [(FilePath, Either String [Match])]
matchFiles exprs paths = mapM (matchFile exprs) paths

matchFile :: [Expr] -> FilePath -> IO (FilePath, Either String [Match])
matchFile exprs name = do
    matches <- loadAndMatch exprs name
    return (name, matches)
    where
        loadAndMatch exprs name = do
            blob <- B.readFile name
            return $ findMatches blob exprs

loadPattern :: Either String String -> IO B.ByteString
loadPattern pat = either B.readFile (return . BI.packChars) pat

showFileMatches :: (FilePath, [Match]) -> IO ()
showFileMatches (path, matches) = mapM_ (putStrLn . showMatch) matches
    where
        showMatch (Match orig (startPos, _) _extract) = emphasized (fileAndPosition startPos) ++ "\n" ++ BI.unpackChars orig -- ++ "\n" ++ (intercalate "\n" $ fmap show extract)
        fileAndPosition (line, pos) = path ++ ":" ++ show (line+1) ++ ":" ++ show (pos+1)

recursiveFiles:: FilePath -> IO [FilePath]
recursiveFiles path = do
    isDir <- doesDirectoryExist path
    if isDir then do
        subdirs <- (fmap (path </>)) <$> listDirectory path
        concat <$> mapM recursiveFiles subdirs
    else
        return [path]

showMatchingFilePaths :: [(FilePath, [Match])] -> IO ()
showMatchingFilePaths matches = mapM_ (putStrLn . fst) matches

-- FIXME check whenever output is actually a terminal
emphasized s = (A.setSGRCode [A.SetColor A.Foreground A.Dull A.Blue]) ++ s ++ A.setSGRCode [A.Reset] 