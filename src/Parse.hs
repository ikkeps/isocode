{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module Parse where

import Control.Applicative
import qualified Data.Word as W
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Numeric (readHex, readOct)


data Expr = Id B.ByteString    -- abcde
          | Op B.ByteString -- */+/-.<> => ->
          | Var W.Word8 Expr      -- @* / $* / %*
          | Block B.ByteString [Expr] B.ByteString -- {*} / [] / ()
          | Sep W.Word8   -- ; or ,
          | Val B.ByteString -- 123.0 / "123abc" / '123abc' / <<EOF..EOF
          | Qw [B.ByteString] -- qw(...)
          | RegExp B.ByteString B.ByteString -- m/.../abc
-- Here goes matching stuff
          | Optional Expr
          | Choice [Expr] 
          | Anything
        deriving (Eq, Show)
        

space :: Parser ()
space = skipWhile isSpace

space1 :: Parser ()
space1 = skip isSpace >> space

isNewLine w = w == 13 || w == 10

isSpace w = isNewLine w || w == 32 || w == 9

anyBracket :: Parser (B.ByteString, B.ByteString)
anyBracket = choice $ fmap (\(start, end) -> (,end) <$> string start) [
          ("(", ")")
        , ("[", "]")
        , ("{", "}")
    ]

parseSep :: Parser Expr
parseSep = Sep <$> (chr ';' <|> chr ',')

parseManyExprs :: Parser [Expr]
parseManyExprs = do
    ignored
    exprs <- iterate []
    ignored
    return $ reverse exprs
    where
        iterate exprs = do
            ignored
            let prev = maybeFirst exprs
            (parseExpr prev >>= \e -> iterate (e:exprs)) <|> return exprs
        maybeFirst (a:_) = Just a
        maybeFirst [] = Nothing


-- prev needed to distinguish regexp from others
parseExpr :: Maybe Expr -> Parser Expr
parseExpr prev = choice [
              parseRegExp prev
            , parseQ
            , parseId
            , parseVar -- $.. @... \...
            , parseBlock -- [ ], ( )
            , parseVal -- 1231 "abc"
            , parseSep
            , hereDocument
            , parseOp  -- + / -
            , parsePrototype -- ($$@$) kind of stuff
            -- FIXME parse tr/../../
            -- FIXME string interpolation extraction
            -- FIXME  =smth
            ]

parseVar = Var <$> varType <*> (parseExpr Nothing)
    where varType = chr '@' <|> chr '$' <|> chr '%'

isOperator = inClass "-<>=&/\\!.*+?^:|~"

parseOp = Op <$> takeWhile1 isOperator

parsePrototype = do
    raw <- src $ chr '(' >> takeWhile1 (inClass "$&;@\\*[]%") >> chr ')'
    return $ Op raw

src p = fst <$> match p

parseId = Id <$> ( src $ (takeWhile1 $ inClass "a-zA-Z_") `sepBy1` (string "::"))

canBeBracket w = isOperator w || inClass "@'\"" w

anySymbolBracket = anyBracket <|> (satisfy canBeBracket >>= \w -> return (B.singleton w, B.singleton w))

parseRegExp Nothing = parseAnyRegExp
parseRegExp (Just (Op _) ) = parseAnyRegExp
parseRegExp (Just (Sep _)) = parseAnyRegExp
parseRegExp (Just _) = opRegExp

opRegExp = do
    string "m" <|> string "qr"
    RegExp <$> quoteBrackets
           <*> regexpFlags

regexpFlags = Data.Attoparsec.ByteString.takeWhile (inClass "a-z")

parseAnyRegExp = opRegExp <|> slashesRegExp
    where
        slashesRegExp = do
            chr '/'
            re <- stringWithEscapesTill (escapeOnly [ BI.c2w '/']) "/"
            f <- regexpFlags
            return $ RegExp re f

parseQ = choice [
        (Val <$> fullyEscapedWithPrefix "qq"),
        (Val <$> fullyEscapedWithPrefix "qx"),
        parseQw,
        (Val <$> (chr 'q' >> quoteBrackets))
        ]

quoteBrackets = do
    space
    (begin, end) <- anySymbolBracket
    stringWithEscapesTill (escapeOnly [B.head begin, B.head end]) end

fullyEscapedWithPrefix prefix = do
    string prefix
    space
    (_, end) <- anySymbolBracket
    stringWithEscapesTill fullEscapeSeq end

parseQw = do
    orig <- fullyEscapedWithPrefix "qw"
    return $ Qw $ fmap BI.packChars $ words $ BI.unpackChars orig

parseVal :: Parser Expr
parseVal = do
    Val <$> (num <|> singleQuotedString <|> doubleQuotedString <|> backQuotedString)
    where
        num = src $ takeWhile1 isPartOfNumber

doubleQuotedString = escapedString "\"" fullEscapeSeq "\""
singleQuotedString = escapedString "\'" (escapeOnly [BI.c2w '\'']) "\'"
backQuotedString = escapedString "`" fullEscapeSeq "`"

hereDocument :: Parser Expr
hereDocument = do
    string "<<"
    (marker, esc) <- choice [
        noQuotes,
        ( (, fail "") <$> singleQuotedString),
        ( (, fullEscapeSeq) <$> doubleQuotedString)
        ]
    space >> chr ';' >> space
    result <- stringWithEscapesTill esc $ B.concat ["\n", marker, "\n"] -- FIXME: SHOULD END EITHER WITH NL OR EOF :(
    return $ Val $ B.concat [result, "\n"] -- Returning \n :(
    where
        noQuotes = do
            Id marker <- parseId
            return $ (marker, fullEscapeSeq)

chr c = word8 $ BI.c2w c

isPartOfNumber w = isDigit w
    || w == (BI.c2w '.')
    || w == (BI.c2w 'b')
    || w == (BI.c2w 'e')
    || w == (BI.c2w 'x')

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

escapedString :: B.ByteString -> Parser W.Word8 -> B.ByteString -> Parser B.ByteString
escapedString begin escapeSeq end = string begin >> stringWithEscapesTill escapeSeq end
      
stringWithEscapesTill :: Parser W.Word8 -> B.ByteString -> Parser B.ByteString
stringWithEscapesTill escapeSeq end = scan
    where 
        scan = do
            choice [
                  (string end >> return "")
                , (B.cons <$> escapeSeq <*> scan) -- FIXME SLOOW
                , (B.cons <$> anyWord8 <*> scan) -- FIXME SLOOW
                ]

fullEscapeSeq :: Parser W.Word8
fullEscapeSeq = do
    chr '\\'
    choice [
        controlCode
      , (string "c" >> anyWord8)
      , hexCode
      , octCode
      , anyWord8 -- We are not really care here
      ]
    where
        controlCode = choice $ fmap (\(char, code) -> chr char >> return (BI.c2w code) ) [
            ('a', '\a'),
            ('b', '\b'),
            ('f', '\f'),
            ('n', '\n'),
            ('r', '\r'),
            ('t', '\t'),
            ('v', '\v')
            ]
        hexCode = do
            chr 'x'
            bs <- src $ count 2 (satisfy $ inClass "0-9a-f")
            let [(code, "")] = readHex $ BI.unpackChars bs 
            return code
        octCode = do
            chr '0'
            bs <- src $ count 2 (satisfy $ inClass "0-7")
            let [(code, "")] = readOct $ BI.unpackChars bs 
            return code

escapeOnly :: [W.Word8] -> Parser W.Word8
escapeOnly symbols = do
    chr '\\'
    satisfy (\w -> elem w symbols)

parseWhole :: Parser [Expr]
parseWhole = do
    exprs <- parseManyExprs
    endOfInput <|> justErrorLine
    return exprs
    
parseFile :: B.ByteString -> Either String [Expr]
parseFile source = parseOnly parseWhole source
        
justErrorLine :: Parser ()
justErrorLine = do
    words <- takeByteString
    fail $ " Can not parse remaining: " ++ BI.unpackChars words
    return ()