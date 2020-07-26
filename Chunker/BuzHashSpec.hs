{-# LANGUAGE OverloadedStrings #-}
module Chunker.BuzHashSpec (spec) where

import Data.ByteString as B
import Test.Hspec

import Chunker.BuzHash

spec :: Spec
spec = do
	describe "buzhash" $ do
		it "passes borgbackup's test suite" $ do
			buzhash borgLookupTable "abcdefghijklmnop" `shouldBe` 3795437769
			buzhash (seededBorgLookupTable 1) "abcdefghijklmnop" `shouldBe` 3795400502
			buzhash (seededBorgLookupTable 1) "abcdefghijklmnop" `shouldBe` buzhashUpdate (seededBorgLookupTable 1) (buzhash (seededBorgLookupTable 1) "Xabcdefghijklmno") (B.head "X") (B.head "p") 16
			buzhash borgLookupTable "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz" `shouldBe` 566521248
