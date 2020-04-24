{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Transform where

import Parse (Expr(..))


t :: [Expr] -> [Expr]
t (Id i : Op "=>": rest) = Choice [ Val i, Id i ] : Op "=>" : t rest
t (Block a exprs b: rest) = Block a (withSepAtEnd exprs) b : t rest
--t (op@(Op "\\") : ovar@(Var t expr) : rest ) = Choice [op >> ovar, Var 123 expr] t rest
t (Val "." : Op "*": rest) = Anything : t rest
t (e : rest) = e : t rest
t [] = []

withSepAtEnd [] = t []
withSepAtEnd exprs@(last -> Sep _ ) = withSepAtEnd $ init exprs
withSepAtEnd exprs = t $ exprs ++ [ Optional ( Choice [Sep 44, Sep 59] ) ]
