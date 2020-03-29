{-# LANGUAGE TupleSections #-}

module Main where

import Text.Pretty.Simple (pPrint)

import System.Directory
import System.FilePath ((</>))
import qualified Options.Applicative as Opt
import Data.Semigroup ((<>))
import Control.Monad (when)
import Control.Applicative ((<**>), (<|>))
import Data.List
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import Parse (parseFile, Expr)
import Matcher (findMatches, Match)
import System.Exit (exitSuccess)


import qualified Data.ByteString.Internal as BI


data Arguments = Arguments
  { example    :: Either FilePath String
  , searchDir  :: String
  , verbose    :: Bool
  , justParse  :: Bool
  }

getArguments :: IO Arguments
getArguments = Opt.execParser commandLineParser

argumentsParser :: Opt.Parser Arguments
argumentsParser = Arguments
    <$> (
            (Left <$> Opt.strArgument (Opt.metavar "EXAMPLE_FILE"))
        <|> (Right <$> Opt.strOption (Opt.short 'e' <> Opt.long "example" <> Opt.metavar "EXPRESSION"))
        )
    <*> Opt.strArgument (Opt.metavar "SEARCH_DIR")
    <*> Opt.switch (Opt.short 'v' <> Opt.long "verbose" <> Opt.help "spew debug information")
    <*> Opt.switch (Opt.short 'j' <> Opt.long "just-parse" <> Opt.help "just parse the example, do not search for it")

commandLineParser :: Opt.ParserInfo Arguments
commandLineParser = Opt.info (argumentsParser <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Searches for code similar to EXAMPLE_FILE or EXPRESSION in SEARCH_DIR"
  <> Opt.header "isacode - search source tree by Perl code sample" )


listDir:: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listDir fileFilter path = do
    subdirs <- (fmap (path </>)) <$> listDirectory path
    concat <$> mapM maybeWalk subdirs
    where
        maybeWalk:: FilePath -> IO [FilePath]
        maybeWalk d = do
            exists <- doesDirectoryExist d
            if exists then (listDir fileFilter d) else return $ filter fileFilter [d]


main :: IO ()
main = do --"/home/spek/tmp/otrs"

    args <- getArguments
    
    blob <- loadExample $ example args  
    
    when (verbose args) $ putStrLn "Parsing..."

    let eitherExprs = parseFile blob

    exprs <- case eitherExprs of
                  Right exprs -> return exprs
                  Left msg -> fail msg

    when (verbose args || justParse args) $ pPrint exprs
    
    when (justParse args) $ exitSuccess    

    when (verbose args) $ putStrLn "Scanning..."

    fileNames <- listDir isPerlFileName (searchDir args)
    
    matches <- mapM (\name -> ( (name,) <$> matchFile exprs name) ) fileNames
    
    let errors = filter isError matches
    putStrLn $ "Scanned " ++ show (length matches) ++ " files / " ++ show (length errors) ++ " errors"
    
    let found = mapMaybe anyMatches matches
    mapM_ (\(name, m) -> putStrLn $ name ++ ":" ++ showMatches m) found
    
    putStrLn $ "Total " ++ show (length found) ++ " files matches"
    putStrLn $ "Total " ++ show (sum $ fmap (length . snd) found) ++ " matches"
    
    where
        isPerlFileName p = any (`isSuffixOf` p) perlExtensions
        perlExtensions = [".pl", ".pm", ".t"]
    
        anyMatches :: (FilePath, Either String [Match]) -> Maybe (FilePath, [Match])
        anyMatches (_, (Right [])) = Nothing
        anyMatches (name, (Right matches)) = Just (name, matches)
        anyMatches (_, (Left _)) = Nothing

        isError (_, (Left _)) = True
        isError _ = False

        matchFile :: [Expr] -> FilePath -> IO (Either String [Match])
        matchFile exprs name = do
            blob <- B.readFile name
            return $ findMatches blob exprs
            
        showMatches matches = (show $ length matches) ++ "\n" ++ (intercalate "\n----\n" $ fmap show matches)

loadExample (Left path) = B.readFile path
loadExample (Right source) = return $ BI.packChars source
