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
import Control.Concurrent.Async.Pool (withTaskGroup, mapConcurrently)
import qualified Transform
import qualified Data.ByteString.Internal as BI


data Arguments = Arguments
  { pattern     :: Either FilePath String
  , searchDir   :: String
  , verbose     :: Bool
  , justParse   :: Bool
  , concurrency :: Int
  , fileNamesOnly :: Bool
  }

getArguments :: IO Arguments
getArguments = Opt.execParser commandLineParser

argumentsParser :: Opt.Parser Arguments
argumentsParser = Arguments
    <$> (
            (Left <$> Opt.strArgument (Opt.metavar "PATTERN_FILE"))
        <|> (Right <$> Opt.strOption (Opt.short 'p' <> Opt.long "pattern" <> Opt.metavar "PATTERN"))
        )
    <*> Opt.strArgument (Opt.metavar "SEARCH_DIR")
    <*> Opt.switch (Opt.short 'v' <> Opt.long "verbose" <> Opt.help "spew debug information")
    <*> Opt.switch (Opt.short 'd' <> Opt.long "debug-pattern" <> Opt.help "just parse the pattern and show, do not search for it")
    <*> Opt.option Opt.auto (Opt.short 'j' <> Opt.long "concurrency" <> Opt.value 4 <> Opt.help "How many threads in parallell")
    <*> Opt.switch (Opt.short 'o' <> Opt.long "filename-only" <> Opt.help "Show only filenames")

commandLineParser :: Opt.ParserInfo Arguments
commandLineParser = Opt.info (argumentsParser <**> Opt.helper)
   ( Opt.fullDesc
  <> Opt.progDesc "Searches for code similar to PATTERN_FILE or PATTERN in SEARCH_DIR"
  <> Opt.header "isocode - search source tree by Perl code sample" )


perlExtensions :: [String]
perlExtensions = [".pl", ".pm", ".t"]


main :: IO ()
main = do

    args <- getArguments
    
    blob <- loadPattern $ pattern args  
    
    when (verbose args) $ putStrLn "Parsing..."

    let eitherExprs = parseFile blob

    exprs <- case eitherExprs of
                  Right exprs -> return exprs
                  Left msg -> fail msg --FIXME proper message

    exprs <- return $ Transform.t exprs

    when (verbose args || justParse args) $ pPrint exprs
    when (justParse args) $ exitSuccess
    when (verbose args) $ putStrLn "Scanning..."

    allFileNames <- listDir (searchDir args)
    
    let fileNames = filter isPerlFileName allFileNames
    
    matches <- matchFiles (concurrency args) exprs fileNames
    
    let found = mapMaybe anyMatches matches

    if fileNamesOnly args then
        showMatchingFilePaths found
    else do
        mapM_ showFileMatches found 
        showStats allFileNames matches found

    where
        isPerlFileName p = any (`isSuffixOf` p) perlExtensions
    
        anyMatches :: (FilePath, Either String [Match]) -> Maybe (FilePath, [Match])
        anyMatches (_, (Right [])) = Nothing
        anyMatches (name, (Right matches)) = Just (name, matches)
        anyMatches (_, (Left _)) = Nothing

showStats :: [FilePath] -> [(FilePath, Either String [Match])] -> [(FilePath, [Match])] -> IO ()
showStats allFileNames matches found = do  
    let errors = filter isError matches
    putStrLn $ "Files in directory: " ++ show (length allFileNames)
    putStrLn $ "Scanned " ++ show (length matches) ++ " files with " ++ show (length errors) ++ " errors"
    putStrLn $ "Total " ++ show (length found) ++ " files matches"
    putStrLn $ "Total " ++ show (sum $ fmap (length . snd) found) ++ " matches"
    where
        isError (_, (Left _)) = True
        isError _ = False

matchFiles :: Int -> [Expr] -> [FilePath] -> IO [(FilePath, Either String [Match])]
matchFiles threadsNum exprs paths = do
    withTaskGroup threadsNum $ \g -> do
        mapConcurrently g (matchFile exprs) paths

matchFile :: [Expr] -> FilePath -> IO (FilePath, Either String [Match])
matchFile exprs name = do
    matches <- loadAndMatch exprs name
    return (name, matches)
    where
        loadAndMatch exprs name = do
            blob <- B.readFile name
            return $ findMatches blob exprs

loadPattern (Left path) = B.readFile path
loadPattern (Right source) = return $ BI.packChars source

showFileMatches :: (FilePath, [Match]) -> IO ()
showFileMatches (path, matches) = mapM_ (putStrLn . showMatch) matches
    where
        showMatch (Match orig ((line, pos), _) _extract) = path ++ ":" ++ show (line+1) ++ ":" ++ show (pos+1) ++ "\n" ++ BI.unpackChars orig
            
listDir:: FilePath -> IO [FilePath]
listDir path = do
    subdirs <- (fmap (path </>)) <$> listDirectory path
    concat <$> mapM maybeWalk subdirs
    where
        maybeWalk:: FilePath -> IO [FilePath]
        maybeWalk d = do
            exists <- doesDirectoryExist d
            if exists then (listDir d) else return [d]


showMatchingFilePaths :: [(FilePath, [Match])] -> IO ()
showMatchingFilePaths matches = mapM_ (putStrLn . fst) matches 