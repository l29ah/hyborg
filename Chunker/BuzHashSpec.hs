{-# LANGUAGE OverloadedStrings #-}
module Chunker.BuzHashSpec (spec) where

import Test.Hspec

import Chunker.BuzHash

spec :: Spec
spec = do
	describe "buzhash" $ do
		it "passes borgbackup's test suite" $ do
			buzhash borgLookupTable "abcdefghijklmnop" `shouldBe` 3795437769
			buzhash (seededBorgLookupTable 1) "abcdefghijklmnop" `shouldBe` 3795400502
