{-# LANGUAGE OverloadedStrings #-}
module CompressionSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Test.Hspec

import Compression

genByteStringS len = BL.toStrict $ BL.take len $ BL.iterate (+ 1) 0

byteString10MS	= genByteStringS 1000000

spec :: Spec
spec = do
	describe "lz4" $ do
		it "compress . decompress == id" $ do
			byteString10MS `shouldBe` (decompress $ BL.toStrict $ compress byteString10MS)
