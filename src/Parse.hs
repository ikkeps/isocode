{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module Parse where

import Control.Applicative
import qualified Data.Word as W
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Numeric (readHex, readOct)
import Control.Monad (when)


data Expr = Id B.ByteString    -- abcde
          | Op B.ByteString -- */+/-.<> => ->
          | Var W.Word8 Expr      -- @* / $* / %*
          | Block B.ByteString [Expr] B.ByteString -- {*} / [] / () FIME: words?
          | Sep W.Word8   -- ; or ,
          | Val B.ByteString -- 123.0 / "123abc" / '123abc' / <<EOF..EOF
          | Qw [B.ByteString] -- qw(...)
          | RegExp B.ByteString B.ByteString -- m/.../abc
          | Tr B.ByteString B.ByteString B.ByteString -- tr/abc/def/g
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
            , parseTr
            , parseQ
            , parseId
            , parseVar -- $.. @... \...
            , parseBlock -- [ ], ( )
            , parseVal -- 1231 "abc"
            , parseSep
            , parseHereDocument
            , parseOp  -- + / -
            , parsePrototype -- ($$@$) kind of stuff
            -- FIXME string interpolation extraction
            -- FIXME  =smth
            ]

parseVar = dollar <|> other
    where
        other = Var <$> (chr '@' <|> chr '%') <*> normalVar
        dollar = Var <$> chr '$'
            <*> (normalVar <|> specialWeird <|> specialCaret <|> specialFilesAndRegexp)
        normalVar = parseId <|> parseBlock <|> parseVar
        specialWeird = (Id . B.singleton) <$> satisfy (inClass "_./,;:#?!@$<>()[]\\0")
        specialCaret = Id <$> src ( chr '^' >> satisfy (inClass "A-Z") )
        specialFilesAndRegexp = (Id . B.singleton) <$> satisfy (inClass "-&`'+|%=~^0-9") -- must go after caret as there is ^

isOperator = inClass "-<>=&/\\!.*+?^:|~"

parseOp = Op <$> takeWhile1 isOperator

parsePrototype = do
    raw <- src $ chr '(' >> takeWhile1 (inClass "$&;@\\*[]%") >> chr ')'
    return $ Op raw

src p = fst <$> match p

parseId = Id <$> ( src $ (takeWhile1 $ inClass "a-zA-Z_") `sepBy1` (string "::"))

canBeBracket w = isOperator w || inClass "@'\"" w

anyBracket :: Parser (W.Word8, W.Word8)
anyBracket = choice $ fmap (\(start, end) -> (,BI.c2w end) <$> word8 (BI.c2w start)) [
          ('(', ')')
        , ('[', ']')
        , ('{', '}')
    ]

anyOneSymbolBracket = satisfy canBeBracket

anySymbolBracket = anyBracket <|> (anyOneSymbolBracket >>= \w -> return (w, w))

parseRegExp Nothing = parseAnyRegExp
parseRegExp (Just (Op _) ) = parseAnyRegExp
parseRegExp (Just (Sep _)) = parseAnyRegExp
parseRegExp (Just _) = opRegExp

opRegExp = do
    string "m" <|> string "qr"
    RegExp <$> (space >> quoteBrackets)
           <*> regexpFlags

regexpFlags = Data.Attoparsec.ByteString.takeWhile (inClass "a-z")

parseAnyRegExp = opRegExp <|> slashesRegExp
    where
        slashesRegExp = do
            chr '/'
            re <- stringWithEscapesTill (escapeOnly [ BI.c2w '/']) (BI.c2w '/')
            f <- regexpFlags
            return $ RegExp re f

parseQ = choice [
        (Val <$> fullyEscapedWithPrefix "qq"),
        (Val <$> fullyEscapedWithPrefix "qx"),
        parseQw,
        (Val <$> (chr 'q' >> space >> quoteBrackets))
    ]

-- This one is tricky as there could be things like tr [abc] /def/ BUT tr"abc"def" and tr/abc/def/
parseTr = do
    string "tr" <|> string "y" -- FIXME "s/// ??"
    space
    withOneSymbolBracket <|> withRegularBrackets
    where
        withOneSymbolBracket = do
            br <- anyOneSymbolBracket
            Tr <$> properEscaping br
               <*> properEscaping br
               <*> regexpFlags
        withRegularBrackets = do
            (_, end) <- anyBracket
            a <- stringWithEscapesTill fullEscapeSeq end
            space
            (_, end) <- anySymbolBracket
            b <- properEscaping end
            flags <- regexpFlags
            return $ Tr a b flags
        properEscaping end = if end == (BI.c2w '\'') then do
                stringWithEscapesTill (escapeOnly [end, (BI.c2w '\\')]) end
            else do
                stringWithEscapesTill fullEscapeSeq end

quoteBrackets = do
    (begin, end) <- anySymbolBracket
    stringWithEscapesTill (escapeOnly [begin, end]) end

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

doubleQuotedString = escapedString '\"' fullEscapeSeq
singleQuotedString = escapedString '\'' (escapeOnly [BI.c2w '\''])
backQuotedString = escapedString '`' fullEscapeSeq

escapedString :: Char -> Parser W.Word8 -> Parser B.ByteString
escapedString quote escapeSeq = chr quote >> stringWithEscapesTill escapeSeq (BI.c2w quote)

parseHereDocument :: Parser Expr
parseHereDocument = do
    string "<<"
    (marker, esc) <- choice [
        noQuotes,
        ( (, fail "") <$> singleQuotedString),
        ( (, fullEscapeSeq) <$> doubleQuotedString)
        ]
    space >> chr ';' >> space
    Val <$> withEscapesTillMarker esc marker
    where
        noQuotes = do
            Id marker <- parseId
            return $ (marker, fullEscapeSeq)
        withEscapesTillMarker esc marker = scan
            where
                scan = do
                    choice [
                          (chr '\n' >> string marker >> ( (chr '\n' >> return "\n") <|> (endOfInput >> return "") ))
                        , (B.cons <$> tryEscape <*> scan) -- FIXME SLOOW
                        , (B.cons <$> anyWord8 <*> scan) -- FIXME SLOOW
                        ]
                tryEscape = do
                    chr '\\'
                    esc <|> return (BI.c2w '\\')

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
    word8 end
    return $ Block (B.singleton start) exprs (B.singleton end)

ignored = skipMany (space1 <|> comment)

stringWithEscapesTill :: Parser W.Word8 -> W.Word8 -> Parser B.ByteString
stringWithEscapesTill escapeSeq end = scan
    where
        scan = do
            eof <- atEnd
            when eof $ fail "unexpected EOF while parsing string"
            (word8 end >> return "") <|> (
                B.append
                    <$> ( (B.singleton <$> tryEscape) <|> (takeTill endOrSlash))
                    <*> scan
                )
            
        endOrSlash w = w == end || w == (BI.c2w '\\')
        tryEscape = do
            chr '\\'
            escapeSeq <|> return (BI.c2w '\\')


fullEscapeSeq :: Parser W.Word8
fullEscapeSeq = do
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
escapeOnly symbols = satisfy (\w -> elem w symbols)

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