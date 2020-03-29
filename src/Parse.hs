{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module Parse where

import Control.Applicative
import qualified Data.Word as W
import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Control.Monad (void)


data Expr = Id B.ByteString    -- abcde
          | Op B.ByteString -- */+/-.<> => ->
          | Var W.Word8 Expr      -- @* / $* / %*
          | Block B.ByteString [Expr] B.ByteString -- {*} / [] / () --FIXME stupid
          | Sep W.Word8   -- ; or , 
          | Val B.ByteString -- 123.0 / "123abc" / '123abc' / <<EOF..EOF
        deriving (Eq, Show)
        

space :: Parser ()
space = skipWhile isSpace

space1 :: Parser ()
space1 = skip isSpace >> space

isNewLine w = w == 13 || w == 10

isSpace w = isNewLine w || w == 32 

anyBracket :: Parser (B.ByteString, B.ByteString)
anyBracket = choice $ fmap (\(start, end) -> (,end) <$> string start) [
          ("(", ")")
        , ("[", "]")
        , ("{", "}")
    ]
    

parseSep :: Parser Expr
parseSep = Sep <$> (chr ';' <|> chr ',')

parseManyExprs :: Parser [Expr]
parseManyExprs = ignored >> parseExpr `sepBy` ignored <* ignored

parseExpr :: Parser Expr
parseExpr = choice [
              (Val <$> parseQ)
            , parseId
            , parseVar -- $.. @... \...
            , parseBlock -- [ ], ( )
            , parseVal -- 1231 "abc"
            , parseSep
            , (Val <$> hereDocument)
            , parseOp  -- + / -
            -- FIXME string interpolation extraction
            -- FIXME  =smth
            -- FIXME regexp - it always goes after op or first statement / after separator
            ]

parseVar = Var <$> varType <*> parseExpr
    where varType = chr '@' <|> chr '$' <|> chr '%'

isOperator = inClass "-<>=&/\\!.*+?^:|~"

parseOp = do
    op <- src prototype <|> takeWhile1 isOperator
    return $ Op op
    where
        prototype = do --FIXME ?
            chr '('
            takeWhile1 $ inClass "$&;@\\*[]"
            chr ')'

src p = fst <$> match p

parseId = Id <$> (takeWhile1 $ inClass "a-zA-Z_:")


anySymbolBracket = anyBracket <|> (satisfy isOperator >>= \w -> return (B.singleton w, B.singleton w))

parseQ = do
    string "qq" <|> string "qw" <|> string "q" <|> string "m"
    (_, end) <- anySymbolBracket
    stringWithEscapesTill end

parseVal :: Parser Expr
parseVal = do
    Val <$> (num <|> singleQuotedString <|> doubleQuotedString <|> backQuotedString)
    where
        num = src $ takeWhile1 isPartOfNumber

doubleQuotedString = escapedString "\"" "\""
singleQuotedString = escapedString "\'" "\'"
backQuotedString = escapedString "`" "`"

hereDocument :: Parser B.ByteString
hereDocument = do
    string "<<"
    marker <- (parseId >>= \(Id marker) -> return marker) <|> singleQuotedString <|> doubleQuotedString  -- FIXME what is difference between " and '?
    ignored
    chr ';'
    ignored
    stringWithEscapesTill marker

chr c = word8 $ BI.c2w c

isPartOfNumber w = isDigit w || w == 0x2e 

isDigit w = w >= 0x30 && w <=0x39

notIsNewLine w = not (w == 10 || w == 13)

comment :: Parser ()
comment = do
    chr '#'
    skipWhile notIsNewLine
    return ()

parseBlock :: Parser Expr 
parseBlock = do
    (start, end) <- anyBracket
    exprs <- parseManyExprs
    string end
    return $ Block start exprs end


ignored = skipMany (space1 <|> comment)

escapedString :: B.ByteString -> B.ByteString -> Parser B.ByteString
escapedString begin end = string begin >> stringWithEscapesTill end
      
stringWithEscapesTill :: B.ByteString -> Parser B.ByteString
stringWithEscapesTill end = do
    content <- src $ loopInString end
    string end -- FIXME kinda lame
    return content
    where 
        loopInString end = do
            choice [
                  (void $ lookAhead $ string end)
                , (escapeSeq >> loopInString end)
                , (anyWord8 >> loopInString end)
                ]

escapeSeq = do
    string "\\"
    choice [
        ((string "x" <|> string "0") >> count 2 (satisfy $ inClass "0-9") >> return ())
      , (string "c" >> anyWord8 >> return ())
      , void anyWord8 -- dont care about others escaped values
        ]

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

