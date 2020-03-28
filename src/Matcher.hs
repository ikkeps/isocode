{-# LANGUAGE OverloadedStrings #-}

module Matcher where

import Parse
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Word as W
import Control.Applicative


findMatches blob exprs = parseOnly (generateMatcher exprs) blob

generateMatcher :: [Expr] -> Parser [Match]
generateMatcher exprs = do
    let shouldMatch = generateMany exprs
    ignored
    let fastForward = skipToFirst (head exprs) -- Small optimization
    res <- many' $ fastForward >> (matchAndSource shouldMatch <|> (anyWord8 >> return []))
    ignored
    return $ concat res
    where
        skipToFirst expr = takeTill (== firstChar expr)

matchAndSource shouldMatch = do
    (orig, m) <- match shouldMatch
    return [Match orig m]

 
generate :: Expr -> Parser [Extract]
generate (Id a) = string a >> return []
generate (Op a) = string a >> return []
generate (Sep a) = string a >> return [] -- FIXME probably should not match separator in some cases
generate orig@(Var kind _expr) = do
    word8 kind
    origExpr <- parseExpr -- <- actually parse anything
    return [VarName (str2bs $ show orig) (str2bs $ show (Var kind origExpr))] -- Lame mapping
generate (Val a) = do
    Val orig <- parseVal
    if orig /= a
        then fail "Val does not match"
        else return []
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


firstChar :: Expr -> W.Word8
firstChar (Id a) = B.head a
firstChar (Op a) = B.head a
firstChar (Sep a) = B.head a
firstChar (Var kind _) = kind
firstChar (Val a) = B.head a
firstChar (Block begin _ _) = B.head begin


