{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Transform where

import Parse (Expr(..))
import Data.List (intersperse)

transform :: [Expr] -> Either String [Expr]
transform exprs = checkErrors $ t exprs

checkErrors :: [Expr] -> Either String [Expr]
checkErrors [Anything] = Left "Can not scan for just anything"
checkErrors (Optional _ : _) = Left "Expression can not start with opional"
checkErrors exprs = Right exprs


t :: [Expr] -> [Expr]
t (Id i : Op "=>": rest) = Choice [ Id i , Val i ] : Op "=>" : t rest
t (Block "(" exprs ")": rest) | isCsv exprs = t $ Qw (unCsv exprs) : rest -- turn regular arrays to qws to turn it to choice later
t (Block a exprs b: rest) = Block a (t $ withSepAtEnd exprs) b : t rest
t (Op "***": rest) = t $ Anything : rest
t (qw@(Qw items): rest) = t $ Choice [ qw, mkArray items ] : rest
t (e : rest) = e : t rest
t [] = []

withSepAtEnd [] = t []
withSepAtEnd exprs@(last -> Sep _ ) = withSepAtEnd $ init exprs
withSepAtEnd exprs = exprs ++ [ Optional ( Choice [comma, Sep 59] ) ]

mkArray vals = Block "(" ( intersperse comma (fmap Val vals) ) ")"

comma = Sep 44

isCsv exprs = all valOrComma exprs
    where
        valOrComma e = isVal e || (e == comma) 

isVal (Val _) = True
isVal _ = False

unCsv exprs = fmap unVal $ filter isVal exprs

unVal (Val v) = v
unVal _ = undefined
