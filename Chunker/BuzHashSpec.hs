{-# LANGUAGE OverloadedStrings #-}
module Chunker.BuzHashSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup
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
	describe "roll" $ do
		it "passes borgbackup's test suite" $ do
			BL.toChunks (roll 0 1 23 2 2 (stimes 3 "foobarboobaz")) `shouldBe` ["fooba", "rboobaz", "fooba", "rboobaz", "fooba", "rboobaz"]
			BL.toChunks (roll 1 1 23 2 2 (stimes 3 "foobarboobaz")) `shouldBe` ["fo", "obarb", "oob", "azf", "oobarb", "oob", "azf", "oobarb", "oobaz"]
			BL.toChunks (roll 2 1 23 2 2 (stimes 3 "foobarboobaz")) `shouldBe` ["foob", "ar", "boobazfoob", "ar", "boobazfoob", "ar", "boobaz"]
			BL.toChunks (roll 0 2 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` [stimes 3 "foobarboobaz"]
			BL.toChunks (roll 1 2 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foobar", "boobazfo", "obar", "boobazfo", "obar", "boobaz"]
			BL.toChunks (roll 2 2 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foob", "arboobaz", "foob", "arboobaz", "foob", "arboobaz"]
			BL.toChunks (roll 0 3 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` [stimes 3 "foobarboobaz"]
			BL.toChunks (roll 1 3 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foobarbo", "obazfoobar", "boobazfo", "obarboobaz"]
			BL.toChunks (roll 2 3 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foobarboobaz", "foobarboobaz", "foobarboobaz"]
