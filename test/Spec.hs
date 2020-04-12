{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import qualified Data.ByteString as B
import Data.String
import Parse

main :: IO ()
main = hspec $ do
    ids
    vars
    values
    ops
    blocks
    regexps
    someExprs

is :: String -> Expr -> Spec
is source result = it source $ parseFile (fromString source) `shouldBe` (Right [result])

isMany :: String -> [Expr] -> Spec
isMany source result = it source $ parseFile (fromString source) `shouldBe` (Right result)


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
    isMany " my ($a,$b) = ()" [
        Id "my",
        Block "(" [
            Var 36 (Id "a"),
            Sep 44,
            Var 36 (Id "b")
          ] ")",
        Op "=",
        Block "(" [] ")"]


ids = describe "ids" $ do
    is "Some::Package::Name" $ Id "Some::Package::Name"
    is "Some" $ Id "Some"
    is "Some_Package" $ Id "Some_Package"
    isMany "Some:Fake::Name" $ [ Id "Some", Op ":", Id "Fake::Name" ]


vars = describe "vars" $ do
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


regexps = describe "regexps" $ do
    is "m/k.e.k/" $ Val "k.e.k"
--FIXME    is "=~ /k.e.k/" $ Val "k.e.k"
    is "m//" $ Val ""


values = describe "values" $ do
    describe "single quotes" $ do
        is "'works'" $ Val "works"
        is " 'wor\\\'ks' " $ Val "wor\\\'ks" -- FIXME unqoute stuff?
        is " '' " $ Val ""

    describe "double quotes" $ do
        is  " \"works\" " $ Val "works"
        is " \"wo\\\"rks\" " $ Val "wo\\\"rks" --FIXME unquote parse?
        is " \"\" " $ Val ""

    describe "qq" $ do
        is "q//" $ Val ""
        is "q( abc )" $ Val " abc "
        is "qq( abc )" $ Val " abc "
        is "qq[ abc ]" $ Val " abc " --FIXME q/qq diff?

-- FIXME    is "qw( ab cd )" $ Block "(" [FIXME] ")"
-- FIXME newline   is "<<SQL;\nabc\nSQL" $ Val "abc" 
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
    -- FIXME more

blocks = describe "blocks" $ do
    is " { a + b }" $ Block "{" [ Id "a", Op "+", Id "b" ] "}"
    is " ( a , b )" $ Block "(" [Id "a", Sep 44, Id "b" ] ")"
    is " [ 123 ] " $ Block "[" [Val "123"] "]"
