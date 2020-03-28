{-# LANGUAGE TupleSections #-}

module Main where

import System.Directory
import System.FilePath ((</>))
import Data.List
import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import Parse (parseFile, Expr, findMatches, Match)

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
    blob <- B.readFile "/home/spek/fun/isocode/examples/should_match.pl"
    let Right exprs = parseFile blob

    putStrLn $ show exprs

    fileNames <- listDir (\p -> (".pl" `isSuffixOf` p) || (".pm" `isSuffixOf` p))  "/home/spek/tmp/otrs" -- "/home/spek/fun/isocode/examples" --  --  -- 
    putStrLn "Parsing..."
    
    matches <- mapM (\name -> ( (name,) <$> matchFile exprs name) ) fileNames
    
    let errors = filter isError matches
    putStrLn $ "Scanned " ++ show (length matches) ++ " files / " ++ show (length errors) ++ " errors"
    
    let found = mapMaybe anyMatches matches
    mapM_ (\(name, m) -> putStrLn $ name ++ ":" ++ showMatches m) found

    putStrLn $ "Total " ++ show (length found) ++ " files matches"
    putStrLn $ "Total " ++ show (sum $ fmap (length . snd) found) ++ " matches"
    
    
    where
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

     
