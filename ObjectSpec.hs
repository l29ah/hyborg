{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module ObjectSpec (spec) where

import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
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
	describe "HMAC" $ do
		it "compatible with borg impl" $ do
			(convert $ hmacGetDigest $ hmac @ByteString @ByteString @SHA512 "1234" "567") `shouldBe` (fst $ B16.decode "9ba02662280064f733fd290584c23779aec475b33819c795ced7aa8a3c616b7bf75c41b4a19e0dca8e4e97ddbffb23b886a4454d208578f3bcc4cc4508ad1c99")
	describe "HKDF" $ do
		it "passes borg test 1" $ do
			let ikm = B.replicate 22 0x0b
			let salt = fst $ B16.decode "000102030405060708090a0b0c"
			let info = fst $ B16.decode "f0f1f2f3f4f5f6f7f8f9"
			let l = 42
			hkdf ikm salt info l `shouldBe` (fst $ B16.decode "832390086cda71fb47625bb5ceb168e4c8e26a1a16ed34d9fc7fe92c1481579338da362cb8d9f925d7cb")
		it "passes borg test 5" $ do
			let ikm = fst $ B16.decode "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c"
			let salt = B.replicate 64 0
			let info = B.empty
			let l = 42
			hkdf ikm salt info l `shouldBe` (fst $ B16.decode "1407d46013d98bc6decefcfee55f0f90b0c7f63d68eb1a80eaf07e953cfc0a3a5240a155d6e4daa965bb")
