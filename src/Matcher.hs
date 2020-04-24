{-# LANGUAGE OverloadedStrings #-}

module Matcher where

import Parse
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Word as W
import Control.Applicative
import qualified Data.Set as Set
    
data Match = Match B.ByteString Cut [Extract]

data Extract = VarName B.ByteString B.ByteString
    deriving Show

type Pos = (Int, Int) -- zero-based
type Cut = (Pos, Pos)

findMatches blob exprs = parseOnly (generateMatcher exprs) blob

type NewlinesAndTail = (Int, Int) -- (newlines, bytes after newline)

data Piece = Extract B.ByteString [Extract]
           | Skip NewlinesAndTail
    deriving Show

generateMatcher :: [Expr] -> Parser [Match]
generateMatcher exprs = do
    let shouldMatch = generateMany exprs
    pieces <- many' $ do
        skip <- fastForward
        mbMatch <- matchAndSource shouldMatch <|> skipWord
        return $ [skip, mbMatch]
    ignored
    return $ piecesToMatches $ concat pieces
    where
        fastForward = do -- Small Optimization
            let firstCharsList = firstChars $ head exprs
            piece <- takeTill (`elem` firstCharsList)
            return $ Skip $ newlinesAndTail piece
        skipWord = do
            w <- anyWord8
            return $ Skip $ if w == 10 then (1,0) else (0,1)

generate :: Expr -> Parser [Extract]
generate (Id a) = string a >> return []
generate (Op a) = string a >> return []
generate (Sep a) = word8 a >> return []
generate orig@(Var kind _expr) = do
    word8 kind
    origExpr <- parseExpr -- <- actually parse anything
    return [VarName (str2bs $ show orig) (str2bs $ show (Var kind origExpr))] -- Lame mapping
generate (Val a) = do
    Val orig <- parseVal
    if orig == a
        then return []
        else fail "Differrent value"
generate (Block begin exprs end) = do
    string begin
    ignored
    extracts <- generateMany exprs
    ignored
    string end
    return extracts
generate (Optional e) = option [] (generate e)
generate (Choice exprs) = choice $ fmap generate exprs
generate Anything = parseManyExprs >> return []


generateMany :: [Expr] -> Parser [Extract]
generateMany exprs = do
    res <- mapM (\e -> ignored >> generate e) exprs -- first ignored is consumed, but its fine
    return $ concat res

str2bs s = B.pack $ fmap BI.c2w s

firstChars :: Expr -> [W.Word8]
firstChars (Id a) = [B.head a]
firstChars (Op a) = [B.head a]
firstChars (Sep a) = [a]
firstChars (Var kind _) = [kind]
firstChars (Val a) = (B.head a) : fmap BI.c2w ['"', '\'', 'q', 'm', '`', '<' ] --FIXME m
firstChars (Block begin _ _) = [B.head begin]
firstChars (Optional _) = error "cannot start with optional characters at the begining" 
firstChars (Choice exprs) = concat $ fmap firstChars exprs
firstChars (Anything) = error "capture anything in beggining is not supported"

isMappingOk :: [Extract] -> Bool
isMappingOk extracts = isAllDifferrent (fmap fst uniq) && isAllDifferrent (fmap snd uniq)
    where
        isAllDifferrent items = Set.size (Set.fromList items) == length items  
        uniq = Set.toList $ Set.fromList $ fmap (\(VarName a b) -> (a,b)) extracts


addNewlineAndTail :: NewlinesAndTail -> NewlinesAndTail -> NewlinesAndTail
addNewlineAndTail (aNls, aTl) (bNls, bTl) = (aNls + bNls, if bNls > 0 then bTl else aTl+bTl)

piecesToMatches :: [Piece] -> [Match]
piecesToMatches pieces = snd $ foldl accWithPositions ((0,0), []) pieces
    where
        accWithPositions :: (NewlinesAndTail, [Match]) -> Piece -> (NewlinesAndTail, [Match])
        accWithPositions (accNlt, matches) (Skip nlt) =
            (addNewlineAndTail accNlt nlt, matches)
        accWithPositions (accNlt, matches) (Extract orig extracts) =
            (newNlt, Match orig (accNlt, newNlt) extracts : matches)
            where newNlt = addNewlineAndTail accNlt $ newlinesAndTail orig


newlinesAndTail :: B.ByteString -> NewlinesAndTail
newlinesAndTail s = (length nlIndices, getTail nlIndices)
    where
        nlIndices = B.elemIndices 10 s
        getTail [] = B.length s
        getTail nls = B.length s - last nls - 1

matchAndSource :: Parser [Extract] -> Parser Piece
matchAndSource shouldMatch = do
    (orig, extracts) <- match shouldMatch
    if isMappingOk extracts
        then return $ Extract orig extracts
        else fail "variables inconsistent"

countNewlines :: B.ByteString -> Int
countNewlines = B.count 13

