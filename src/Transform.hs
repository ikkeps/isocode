{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Transform where

import Parse (Expr(..))


transform :: [Expr] -> Either String [Expr]
transform exprs = checkErrors $ t exprs

t :: [Expr] -> [Expr]
t (Id i : Op "=>": rest) = Choice [ Val i, Id i ] : Op "=>" : t rest
t (Block a exprs b: rest) = Block a (t $ withSepAtEnd exprs) b : t rest
t (Op "***": rest) = t $ Anything : rest
t (e : rest) = e : t rest
t [] = []

withSepAtEnd [] = t []
withSepAtEnd exprs@(last -> Sep _ ) = withSepAtEnd $ init exprs
withSepAtEnd exprs = exprs ++ [ Optional ( Choice [Sep 44, Sep 59] ) ]

checkErrors :: [Expr] -> Either String [Expr]
checkErrors [Anything] = Left "Can not scan for just anything"
checkErrors (Optional _ : _) = Left "Expression can not start with opional"
checkErrors exprs = Right exprs
