{-# LANGUAGE OverloadedStrings #-}
module ParseStrings where

import Control.Applicative
import Data.Attoparsec.ByteString
import Control.Monad (void)
import qualified Data.ByteString as B

escapedString :: B.ByteString -> B.ByteString -> Parser ()
escapedString begin end = do
    string begin
    inString
    where
        inString = do
            void $ manyTill anyWord8 ((void $ string end) <|> (escapeSeq >> inString))
        escapeSeq = do
            string "\\"
            choice [
                ((string "x" <|> string "0") >> count 2 (satisfy $ inClass "0-9") >> return ())
              , (string "c" >> anyWord8 >> return ())
              , void anyWord8 -- dont care about others escaped values
                ]
