{-# LANGUAGE OverloadedStrings #-}

module Matcher where

import Parse
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Word as W
import Control.Applicative


findMatches blob exprs = parseOnly (generateMatcher exprs) blob

-- findMatches blob exprs = result $ parseFile blob
   -- where result (Right given) = if given == exprs then Right [Match blob []] else Right []
     --     result (Left err) = Left err

generateMatcher :: [Expr] -> Parser [Match]
generateMatcher exprs = do
    let shouldMatch = generateMany exprs
    ignored
    let firstCharsList = firstChars $ head exprs
    let fastForward = skipToFirst firstCharsList -- Small optimization
    res <- many' $ fastForward >> (matchAndSource shouldMatch <|> (anyWord8 >> return []))
    ignored
    return $ concat res
    where
        skipToFirst firstCharsList = takeTill (`elem` firstCharsList)

matchAndSource shouldMatch = do
    (orig, m) <- match shouldMatch
    return [Match orig m]

 
generate :: Expr -> Parser [Extract]
generate (Id a) = string a >> return []
generate (Op a) = string a >> return []
generate (Sep a) = word8 a >> return [] -- FIXME probably should not match separator in some cases
generate orig@(Var kind _expr) = do
    word8 kind
    origExpr <- parseExpr -- <- actually parse anything
    return [VarName (str2bs $ show orig) (str2bs $ show (Var kind origExpr))] -- Lame mapping
generate (Val a) = do
    Val orig <- parseVal
    if orig == a
        then return []
        else fail "Wrong value"
generate (Block begin exprs end) = do
    string begin
    ignored
    extracts <- generateMany exprs
    ignored
    string end
    return extracts

generateMany :: [Expr] -> Parser [Extract]
generateMany exprs = do
    res <- mapM (\e -> ignored >> generate e) exprs -- first ignored is consumed, but its fine
    return $ concat res

str2bs s = B.pack $ fmap BI.c2w s

data Extract = VarName B.ByteString B.ByteString
    deriving Show
    
data Match = Match B.ByteString [Extract]


instance Show Match where
    show (Match orig _extracts) = BI.unpackChars orig


firstChars :: Expr -> [W.Word8]
firstChars (Id a) = [B.head a]
firstChars (Op a) = [B.head a]
firstChars (Sep a) = [a]
firstChars (Var kind _) = [kind]
firstChars (Val a) = (B.head a) : fmap BI.c2w ['"', '\'', 'q', 'm', '`', '<' ] --FIXME m
firstChars (Block begin _ _) = [B.head begin]


