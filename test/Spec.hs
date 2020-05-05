{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import Data.String (fromString)
import Data.Either (isLeft)
import Parse

import qualified MatchingTests as M

main :: IO ()
main = hspec $ do
    ids
    variables
    values
    ops
    blocks
    regexps
    someExprs
    replaces
    M.spec

is :: String -> Expr -> Spec
is source result = it source $ parseFile (fromString source) `shouldBe` (Right [result])

isMany :: String -> [Expr] -> Spec
isMany source result = it source $ parseFile (fromString source) `shouldBe` (Right result)

isFail source = it source $ parseFile (fromString source) `shouldSatisfy` isLeft

someExprs = describe "expressions" $ do
    isMany " if ( $a == $b ) { print 'abc'; }" [
          Id "if"
        , Block "(" [
            Var 36 (Id "a"),
            Op "==",
            Var 36 (Id "b")
          ] ")"
        , Block "{" [
            Id "print", Val "abc",
            Sep 59
          ] "}"
        ]

    isMany " my ( $a, %b ) = ();" [
        Id "my",
        Block "(" [
            Var 36 (Id "a"),
            Sep 44,
            Var 37 (Id "b")
          ] ")",
        Op "=",
        Block "(" [] ")",
        Sep 59
        ]


ids = describe "ids" $ do
    is "Some::Package::Name" $ Id "Some::Package::Name"
    is "Some" $ Id "Some"
    is "Some_Package" $ Id "Some_Package"
    isMany "Some:Fake::Name" $ [ Id "Some", Op ":", Id "Fake::Name" ]


variables = describe "variables" $ do
    describe "special variables" $ do -- https://www.tutorialspoint.com/perl/perl_special_variables.htm
        is "$_" $ Var 36 (Id "_")
        is "$." $ Var 36 (Id ".") -- FIXME should it be Id?
        is "$/" $ Var 36 (Id "/")
        is "$," $ Var 36 (Id ",")
        is "$\\" $ Var 36 (Id "\\")
        is "$;" $ Var 36 (Id ";")
        is "$^L" $ Var 36 (Id "^L")
        is "$:" $ Var 36 (Id ":")
        is "$^A" $ Var 36 (Id "^A")
        is "$#" $ Var 36 (Id "#")
        is "$?" $ Var 36 (Id "?")
        is "$!" $ Var 36 (Id "!")
        is "$@" $ Var 36 (Id "@")
        is "$$" $ Var 36 (Id "$")
        is "$<" $ Var 36 (Id "<")
        is "$>" $ Var 36 (Id ">")
        is "$(" $ Var 36 (Id "(")
        is "$)" $ Var 36 (Id ")")
        is "$0" $ Var 36 (Id "0")
        is "$[" $ Var 36 (Id "[")
        is "$]" $ Var 36 (Id "]")
        is "$^D" $ Var 36 (Id "^D")
        is "$^E" $ Var 36 (Id "^E")
        is "$^F" $ Var 36 (Id "^F")
        is "$^H" $ Var 36 (Id "^H")
        is "$^I" $ Var 36 (Id "^I")
        is "$^M" $ Var 36 (Id "^M")
        is "$^O" $ Var 36 (Id "^O")
        is "$^P" $ Var 36 (Id "^P")
        is "$^T" $ Var 36 (Id "^T")
        is "$^W" $ Var 36 (Id "^W")
        is "$^X" $ Var 36 (Id "^X")

    describe "special variables regular experssion" $ do
        is "$0" $ Var 36 (Id "0")
        is "$1" $ Var 36 (Id "1")
        is "$&" $ Var 36 (Id "&")
        is "$`" $ Var 36 (Id "`")
        is "$'" $ Var 36 (Id "'")
        is "$+" $ Var 36 (Id "+")

    describe "special variables file handle" $ do
        is "$|" $ Var 36 (Id "|")
        is "$%" $ Var 36 (Id "%")
        is "$=" $ Var 36 (Id "=")
        is "$-" $ Var 36 (Id "-")
        is "$~" $ Var 36 (Id "~")
        is "$^" $ Var 36 (Id "^")


    describe "alowed chars" $ do
        is " $a_b " $ Var 36 (Id "a_b")
        is " $_b " $ Var 36 (Id "_b")
        is " $SOME_THING " $ Var 36 (Id "SOME_THING")
    describe "types" $ do
        is " $abc " $ Var 36 (Id "abc")
        is " @abc " $ Var 64 (Id "abc")
        is " %abc " $ Var 37 (Id "abc")
        is " %$abc " $ Var 37 (Var 36 (Id "abc"))
        is " $$abc " $ Var 36 (Var 36 (Id "abc"))
    describe "ref is just operator" $ do
        isMany "\\$abc" [ Op "\\", Var 36 (Id "abc") ] 


regexps = describe "regexps" $ do
    describe "m or //" $ do
        is "m/k.e.k/" $ RegExp "k.e.k" ""
        is "/k.e.k/" $ RegExp "k.e.k" ""
        isMany " =~ /k.e.k/ " $ [Op "=~", RegExp "k.e.k" ""]
        isMany " !~ /k.e.k/ " $ [Op "!~", RegExp "k.e.k" ""]
        isMany "; /k.e.k/ " $ [Sep 59, RegExp "k.e.k" ""]
        is "m //" $ RegExp "" ""
        is "m [ abc ]" $ RegExp " abc " ""
        isMany "$a / $b" $ [ Var 36 (Id "a"), Op "/",  Var 36 (Id "b")]
        isMany "a / $b" $ [ Id "a", Op "/",  Var 36 (Id "b")]
        is "m\t/ab/cd" $ RegExp "ab" "cd"
        is "m/[a-z]/io" $ RegExp "[a-z]" "io"
        is "m '[a-z]'io" $ RegExp "[a-z]" "io"
    describe "qr" $ do
        is "qr/k.e.k/" $ RegExp "k.e.k" ""
        isMany " =~ qr(k.e.k) " $ [Op "=~", RegExp "k.e.k" ""]
        isMany "; qr/k.e.k/ " $ [Sep 59, RegExp "k.e.k" ""]
        is "qr //" $ RegExp "" ""
        is "qr [ abc ]" $ RegExp " abc " ""
        is "qr\t/ab/cd" $ RegExp "ab" "cd"
        is "qr/[a-z]/io" $ RegExp "[a-z]" "io"
        is "qr '[a-z]'io" $ RegExp "[a-z]" "io"



values = describe "values" $ do
    describe "single quotes" $ do
        isFail " 'work "
        is "'works'" $ Val "works"
        is " 'wor\\'ks' " $ Val "wor'ks"
        is " 'wor\\nks' " $ Val "wor\\nks"
        is " 'wor\\ks' " $ Val "wor\\ks"
        is " '' " $ Val ""

    describe "double quotes" $ do
        isFail " \"work "
        is " \"\" " $ Val ""
        is  " \"works\" " $ Val "works"
        is  " \"wo\\x21rks\" " $ Val "wo!rks"
        is  " \"wo\\041rks\" " $ Val "wo!rks"
        is  " \"wo\\c#rks\" " $ Val "wo#rks"
        is " \"wo\\\"rks\" " $ Val "wo\"rks"
        is " \"wo\\nks\" " $ Val "wo\nks"
        is " \"wo\\\\ks\" " $ Val "wo\\ks"
        is " \"\" " $ Val ""
    describe "weird quotes" $ do
        isFail " `test "
        is " `` " $ Val ""
        is " `works` " $ Val "works"
        is " `wo\\x21rks` " $ Val "wo!rks"
        is " `wo\\041rks` " $ Val "wo!rks"
        is " `wo\\c#rks` " $ Val "wo#rks"
        is " `wo\\\"rks` " $ Val "wo\"rks"
        is " `wo\\nks` " $ Val "wo\nks"
        is " `wo\\\\ks` " $ Val "wo\\ks"
        is " `` " $ Val ""


    describe "q" $ do
        is "q//" $ Val ""
        is "q ( abc )" $ Val " abc "
        is "q( ab\\(c\\) )" $ Val " ab(c) "
        is "q( ab\\nc\\ )" $ Val " ab\\nc\\ "
        is "q ' ab\\nc\\ '" $ Val " ab\\nc\\ "

    describe "qq" $ do
        is "qq( abc )" $ Val " abc "
        is "qq( a\\)bc )" $ Val " a)bc "
        is "qq( \\(a\\)bc )" $ Val " (a)bc "
        is "qq ( ab\\nc )" $ Val " ab\nc "
        is "qq [ abc ]" $ Val " abc "
        is "qq ' abc '" $ Val " abc "

    describe "qx" $ do
        is "qx( abc )" $ Val " abc "
        is "qx ( a\\)bc )" $ Val " a)bc "
        is "qx( \\(a\\)bc )" $ Val " (a)bc "
        is "qx( ab\\nc )" $ Val " ab\nc "
        is "qx[ abc ]" $ Val " abc "
        is "qx' abc '" $ Val " abc "

    describe "qw" $ do
        is "qw( ab cd )" $ Qw ["ab", "cd"]
        is "qw/ ab cd /" $ Qw ["ab", "cd"]
        is "qw//" $ Qw []
        is "qw/ 'ab' (cd)/" $ Qw ["'ab'", "(cd)"]
        is "qw[ k\"ek ]" $ Qw ["k\"ek"]
        is "qw[ kek \n \n lol ]" $ Qw ["kek", "lol"]

    describe "here document" $ do -- Note: newline is part of value in perl
        isMany "<<SQL;\nnot\nterminated" [Op "<<",Id "SQL",Sep 59,Id "not", Id "terminated"]
        isMany "<<SQL;\nnotterminated" [Op "<<",Id "SQL",Sep 59,Id "notterminated"]
        is "<<SQL;\nabc\nSQL" $ Val "abc"
        is "<<SQL;\nabc\nSQL\n" $ Val "abc\n"
        is "<<'SQL';\nabc\nSQL\n" $ Val "abc\n"
        is "<<'SQL';\na\\nbc\nSQL\n" $ Val "a\\nbc\n"
        is "<<\"SQL\";\nabc\nSQL\n" $ Val "abc\n"
        is "<<\"SQL\";\na\\nbc\nSQL\n" $ Val "a\nbc\n"

    describe "numbers" $ do
        is "1234" $ Val "1234"
        is "0b1110011" $ Val "0b1110011"
        is "01234" $ Val "01234"
        is "0x1234" $ Val "0x1234"
        is ".123" $ Val ".123"
        isMany "x123" [Id "x", Val "123"]
-- FIXME        isMany "0xxx123" $ [Val "0", Id "xxx", Val "123"]
-- FIXME        is "12.34e-56" $ Val "12.34e-56"
        isMany "-12.34e56" [Op "-", Val "12.34e56"]

ops = describe "ops" $ do
    is " + " $ Op "+"
    is " << " $ Op "<<"
    isMany "a ? b : c" [ Id "a", Op "?", Id "b", Op ":", Id "c" ] 
    isMany "a ? b:c" [ Id "a", Op "?", Id "b", Op ":", Id "c" ] 
    isMany "a==b" [Id "a", Op "==", Id "b"]
    -- FIXME more

blocks = describe "blocks" $ do
    is " { a + b }" $ Block "{" [ Id "a", Op "+", Id "b" ] "}"
    is " ( a , b )" $ Block "(" [Id "a", Sep 44, Id "b" ] ")"
    is " [ 123 ] " $ Block "[" [Val "123"] "]"


replaces = describe "tr / y" $ do
    describe "escaping" $ do
        is "tr'\\\\ab\\nc'def'g" $ Tr "\\ab\\nc" "def" "g"
        is "tr/\\\\ab\\nc/def/g" $ Tr "\\ab\nc" "def" "g"
    describe "regular" $ do
        is "tr/abc/def/g " $ Tr "abc" "def" "g"
        is "tr /abc/def/ " $ Tr "abc" "def" ""
        is "y/abc/def/g " $ Tr "abc" "def" "g"
        is "y /abc/def/ " $ Tr "abc" "def" ""
    describe "weird" $ do
        is "tr(abc) [def]g" $ Tr "abc" "def" "g"
        is "tr(abc) /def/g" $ Tr "abc" "def" "g"
        is "tr(abc) 'def'g" $ Tr "abc" "def" "g"
        is "y(abc) [def]g" $ Tr "abc" "def" "g"
        is "y(abc) /def/g" $ Tr "abc" "def" "g"
        is "y(abc) 'def'g" $ Tr "abc" "def" "g"
