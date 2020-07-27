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
	describe "chunkify" $ do
		it "chunks over max size correctly" $ do
			let bigdata = (BL.replicate (2^23 * 3 `div` 2) 0) <> "Y"
			let chunked = BL.toChunks $ chunkify 0 1 23 2 2 $ bigdata
			length chunked `shouldBe` 2
			BL.fromChunks chunked `shouldBe` bigdata
		it "chunks near max size like borgbackup's impl" $ do
			BL.toChunks (chunkify 2 1 3 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foo", "ba", "rboobazf", "oo", "ba", "rboobazf", "oo", "ba", "rboobaz"]
		it "passes borgbackup's test suite" $ do
			BL.toChunks (chunkify 0 1 23 2 2 (stimes 3 "foobarboobaz")) `shouldBe` ["fooba", "rboobaz", "fooba", "rboobaz", "fooba", "rboobaz"]
			BL.toChunks (chunkify 1 1 23 2 2 (stimes 3 "foobarboobaz")) `shouldBe` ["fo", "obarb", "oob", "azf", "oobarb", "oob", "azf", "oobarb", "oobaz"]
			BL.toChunks (chunkify 2 1 23 2 2 (stimes 3 "foobarboobaz")) `shouldBe` ["foob", "ar", "boobazfoob", "ar", "boobazfoob", "ar", "boobaz"]
			BL.toChunks (chunkify 0 2 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` [stimes 3 "foobarboobaz"]
			BL.toChunks (chunkify 1 2 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foobar", "boobazfo", "obar", "boobazfo", "obar", "boobaz"]
			BL.toChunks (chunkify 2 2 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foob", "arboobaz", "foob", "arboobaz", "foob", "arboobaz"]
			BL.toChunks (chunkify 0 3 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` [stimes 3 "foobarboobaz"]
			BL.toChunks (chunkify 1 3 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foobarbo", "obazfoobar", "boobazfo", "obarboobaz"]
			BL.toChunks (chunkify 2 3 23 2 3 (stimes 3 "foobarboobaz")) `shouldBe` ["foobarboobaz", "foobarboobaz", "foobarboobaz"]
