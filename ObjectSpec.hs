{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module ObjectSpec (spec) where

import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.ByteArray
import Data.ByteString (ByteString)
import Data.Either
import Test.QuickCheck.Instances.ByteString ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

import Object

str :: BL.ByteString
str = BL.take 6666666 $ BL.cycle "our fancy test string"

b16 = fromRight undefined . B16.decode

spec :: Spec
spec = do
	describe "plaintext crypto (and lz4)" $ do
		modifyMaxSuccess (const 10000) $ it "decrypt . encrypt == id" $ property $
			\s -> (decrypt $ BL.toStrict $ encrypt plaintext s) `shouldBe` Just (BL.toStrict s)
		it "decrypt . encrypt == id, a long hardcoded string" $ do
			(decrypt $ BL.toStrict $ encrypt plaintext str) `shouldBe` Just (BL.toStrict str)
	describe "HMAC" $ do
		it "compatible with borg impl" $ do
			(convert $ hmacGetDigest $ hmac @ByteString @ByteString @SHA512 "1234" "567") `shouldBe` (b16 "9ba02662280064f733fd290584c23779aec475b33819c795ced7aa8a3c616b7bf75c41b4a19e0dca8e4e97ddbffb23b886a4454d208578f3bcc4cc4508ad1c99")
	describe "HKDF" $ do
		it "passes borg test 1" $ do
			let ikm = B.replicate 22 0x0b
			let salt = b16 "000102030405060708090a0b0c"
			let info = b16 "f0f1f2f3f4f5f6f7f8f9"
			let l = 42
			hkdf ikm salt info l `shouldBe` (b16 "832390086cda71fb47625bb5ceb168e4c8e26a1a16ed34d9fc7fe92c1481579338da362cb8d9f925d7cb")
		it "passes borg test 2" $ do
			let ikm = b16 "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f"
			let salt = b16 "606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
			let info = b16 "b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
			let l = 82
			hkdf ikm salt info l `shouldBe` (b16 "ce6c97192805b346e6161e821ed165673b84f400a2b514b2fe23d84cd189ddf1b695b48cbd1c8388441137b3ce28f16aa64ba33ba466b24df6cfcb021ecff235f6a2056ce3af1de44d572097a8505d9e7a93")
		it "passes borg test 3" $ do
			let ikm = b16 "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
			let salt = B.replicate 64 0
			let info = B.empty
			let l = 42
			hkdf ikm salt info l `shouldBe` (b16 "f5fa02b18298a72a8c23898a8703472c6eb179dc204c03425c970e3b164bf90fff22d04836d0e2343bac")
		it "passes borg test 4" $ do
			let ikm = b16 "0b0b0b0b0b0b0b0b0b0b0b"
			let salt = b16 "000102030405060708090a0b0c"
			let info = b16 "f0f1f2f3f4f5f6f7f8f9"
			let l = 42
			hkdf ikm salt info l `shouldBe` (b16 "7413e8997e020610fbf6823f2ce14bff01875db1ca55f68cfcf3954dc8aff53559bd5e3028b080f7c068")
		it "passes borg test 5" $ do
			let ikm = b16 "0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c"
			let salt = B.replicate 64 0
			let info = B.empty
			let l = 42
			hkdf ikm salt info l `shouldBe` (b16 "1407d46013d98bc6decefcfee55f0f90b0c7f63d68eb1a80eaf07e953cfc0a3a5240a155d6e4daa965bb")
