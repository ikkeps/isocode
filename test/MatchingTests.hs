{-# LANGUAGE OverloadedStrings #-}

module MatchingTests where

import Matcher

import Test.Hspec

spec = do
    positionAcc
    nlAndTail 

positionAcc = describe "position add" $ do
    it "0+0" $ addNewlineAndTail (0,0) (0,0) `shouldBe` (0,0)
    it "0+1" $ addNewlineAndTail (0,0) (0,5) `shouldBe` (0,5)
    it "1+1" $ addNewlineAndTail (0,3) (0,5) `shouldBe` (0,8)
    it "0+1 with nls" $ addNewlineAndTail (0,0) (3,5) `shouldBe` (3,5)
    it "1+0" $ addNewlineAndTail (5,10) (0,0) `shouldBe` (5, 10)
    it "1+1 with nls" $ addNewlineAndTail (5,10) (0,3) `shouldBe` (5, 13)
    it "1+1 both nls" $ addNewlineAndTail (5,10) (4,3) `shouldBe` (9, 3)
    it "1+0 both nls" $ addNewlineAndTail (5,10) (4,0) `shouldBe` (9, 0)

nlAndTail = describe "nls and tail" $ do

    it "empty" $ newlinesAndTail "" `shouldBe` (0,0)
    it "no nls" $ newlinesAndTail "abcd" `shouldBe` (0,4)
    it "with 1 nl" $ newlinesAndTail "ab\n" `shouldBe` (1,0)
    it "with some nls" $ newlinesAndTail "ab\ncd\nefg" `shouldBe` (2,3)
    it "right at nl" $ newlinesAndTail "\nabcd" `shouldBe` (1,4)
