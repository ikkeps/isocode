{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.List
import Control.Applicative
import qualified Data.Word as W
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Control.Monad (void)

import ParseStrings (escapedString)

data Expr = Id B.ByteString    -- abcde
          | Op B.ByteString -- */+/-.<> => ->
          | Var W.Word8 Expr      -- @* / $*
          | Block B.ByteString [Expr] B.ByteString -- {*} / [] / () --FIXME stupid
          | Sep B.ByteString   -- ; or ,
          | Val B.ByteString -- 123.0 / "123abc" / '123abc'
        deriving (Eq)
        
instance Show Expr where
    show (Id a) = BI.unpackChars a
    show (Op a) = BI.unpackChars a
    show (Sep a) = BI.unpackChars a
    show (Var kind expr) = [BI.w2c kind] ++ show expr 
    show (Val a) = BI.unpackChars a
    show (Block open exprs close) = BI.unpackChars open ++"\n" ++ (showExprs exprs) ++ "\n" ++ BI.unpackChars close
    
showExprs :: [Expr] -> String
showExprs exprs = Data.List.intercalate " " $ fmap show exprs


space :: Parser ()
space = skipWhile isSpace

space1 :: Parser ()
space1 = skip isSpace >> space
    
isSpace w = w == 13 || w == 10 || w == 32 

parseSep :: Parser Expr
parseSep = Sep <$> (string ";" <|> string ",")

parseManyExprs :: Parser [Expr]
parseManyExprs = ignored >> parseExpr `sepBy` ignored <* ignored

parseExpr :: Parser Expr
parseExpr = choice [
              parseId
            , parseVar
            , parseOp
            , parseVal
            , parseBlock "{" "}"
            , parseBlock "(" ")"
            , parseBlock "[" "]"
            , parseSep
            -- FIXME  =smth
            -- FIXME <<<"SQL" ... SQL
            ]

parseVar = Var <$> varType <*> parseExpr
    where varType = word8 (BI.c2w '@') <|> word8 (BI.c2w '$') <|> word8 (BI.c2w '%')

parseOp = do
    op <- choice [
          (src regexpMatch)
        , (src prototype)
        , (takeWhile1 $ inClass "-<>=&/\\!.*+?^:|~")
        ]
    return $ Op op
    where
        prototype = do --FIXME ?
            string "("
            takeWhile1 $ inClass "$&;@\\*[]"
            string ")"
        regexpMatch = do
           (string "=~" <|> string "!~")
           space
           option "" (string "m")
           (parseBlock "/" "/" <|> parseBlock "(" ")" <|> parseBlock "{" "}") --FIXME hack :(

src p = fst <$> match p

parseId = Id <$> (takeWhile1 $ inClass "a-zA-Z_:")

parseVal :: Parser Expr
parseVal = do
    Val <$> src (num <|> singleQuotedString <|> doubleQuotedString)
    where
        num = void $ takeWhile1 isPartOfNumber
        doubleQuotedString = escapedString "\"" "\""
        singleQuotedString = escapedString "\'" "\'"

chr c = word8 $ BI.c2w c

isPartOfNumber w = isDigit w || w == 0x2e 

isDigit w = w >= 0x30 && w <=0x39

notIsNewLine w = not (w == 10 || w == 13)
 
comment = do
    chr '#'
    skipWhile notIsNewLine
    return ()

parseBlock start end = do
    string start
    exprs <- parseManyExprs
    string end
    return $ Block start exprs end


parseWhole :: Parser [Expr]
parseWhole = do
    exprs <- parseManyExprs
    endOfInput <|> justErrorLine
    return exprs
    
parseFile :: B.ByteString -> Either String [Expr]
parseFile source = parseOnly parseWhole source
        
justErrorLine = do
    lines <- count 4 $ manyTill anyWord8 (string "\n" <|> (endOfInput >> return ""))
    fail $ show $ fmap (BI.unpackChars . B.pack) lines
        
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

ignored = skipMany (space1 <|> comment)

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


generate :: Expr -> Parser [Extract]
generate (Id a) = string a >> return []
generate (Op a) = string a >> return []
generate (Sep a) = string a >> return []
generate orig@(Var kind _expr) = do
    word8 kind
    origExpr <- parseExpr -- <- actually parse anything
    return [VarName (str2bs $ show orig) (str2bs $ show (Var kind origExpr))]
generate (Val a) = string a >> return []
generate (Block begin exprs end) = do
    string begin
    ignored
    extracts <- generateMany exprs
    ignored
    string end
    return extracts
