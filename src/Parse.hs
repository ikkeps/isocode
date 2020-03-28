{-# LANGUAGE OverloadedStrings #-}

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
          | Sep B.ByteString   -- ; or , 
          | Val B.ByteString -- 123.0 / "123abc" / '123abc' / <<EOF..EOF
        deriving (Eq, Show)
        

space :: Parser ()
space = skipWhile isSpace

space1 :: Parser ()
space1 = skip isSpace >> space

isNewline w = w == 13 || w == 10

isSpace w = isNewLine w || w == 32 

parseSep :: Parser Expr
parseSep = Sep <$> (string ";" <|> string ",")

parseManyExprs :: Parser [Expr]
parseManyExprs = ignored >> parseExpr `sepBy` ignored <* ignored

parseExpr :: Parser Expr
parseExpr = choice [
              parseId
            , parseVar
            , parseVal
            , parseOp
            , parseBlock "{" "}"
            , parseBlock "(" ")"
            , parseBlock "[" "]"
            , parseSep
            -- FIXME  =smth
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
    Val <$> (num <|> singleQuotedString <|> doubleQuotedString <|> hereDocument)
    where
        num = src $ takeWhile1 isPartOfNumber

doubleQuotedString = escapedString "\"" "\""
singleQuotedString = escapedString "\'" "\'"

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

parseBlock :: B.ByteString -> B.ByteString -> Parser Expr 
parseBlock start end = do
    string start
    exprs <- parseManyExprs
    string end
    return $ Block start exprs end


escapedString :: B.ByteString -> B.ByteString -> Parser B.ByteString
escapedString begin end = string begin >> stringWithEscapesTill end
      
stringWithEscapesTill :: B.ByteString -> Parser B.ByteString
stringWithEscapesTill end = do
    content <- src $ manyTill anyWord8 (lookAhead (string end) <|> (escapeSeq >> stringWithEscapesTill end))
    string end -- FIXME kinda lame
    return content

ignored = skipMany (space1 <|> comment)

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

