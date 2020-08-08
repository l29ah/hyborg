{-# LANGUAGE OverloadedStrings #-}
module Types.GenericsSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.MessagePack
import Data.Semigroup
import Test.Hspec

import Types

ai = ArchiveItem [] 1 2 3 4 "5" 6 "7" True "9" 10

spec :: Spec
spec = do
	describe "gToObjectMap" $ do
		it "serializes ArchiveItem correctly" $ do
			toObject ai `shouldBe` ObjectMap [(ObjectBin "atime",ObjectWord 1),(ObjectBin "chunks",ObjectArray []),(ObjectBin "ctime",ObjectWord 2),(ObjectBin "gid",ObjectWord 4),(ObjectBin "group",ObjectBin "5"),(ObjectBin "hardlink_master",ObjectBool True),(ObjectBin "mtime",ObjectWord 3),(ObjectBin "path",ObjectBin "9"),(ObjectBin "size",ObjectWord 10),(ObjectBin "uid",ObjectWord 6),(ObjectBin "user",ObjectBin "7")]
		it "id via ArchiveItem" $ do
			(fromJust $ fromObject $ toObject ai) `shouldBe` ai
