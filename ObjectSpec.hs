{-# LANGUAGE OverloadedStrings #-}
module ObjectSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Test.Hspec

import Object

str :: BL.ByteString
str = BL.take 6666666 $ BL.cycle "our fancy test string"

spec :: Spec
spec = do
	describe "plaintext crypto (and lz4)" $ do
		it "decrypt . encrypt == id" $ do
			(decrypt $ BL.toStrict $ encrypt plaintext str) `shouldBe` Just (BL.toStrict str)
