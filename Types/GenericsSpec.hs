{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Types.GenericsSpec (spec) where

import Data.MessagePack
import Test.Hspec

import Types

instance MonadFail (Either String) where
	fail = Left

ai :: ArchiveItem
ai = ArchiveItem [] 1 2 3 4 "5" 6 "7" 8 True "9" 10

spec :: Spec
spec = do
	describe "gToObjectMap" $ do
		it "serializes ArchiveItem correctly" $ do
			gToObjectMap ai `shouldBe` ObjectMap [(ObjectBin "atime",ObjectWord 1),(ObjectBin "chunks",ObjectArray []),(ObjectBin "ctime",ObjectWord 2),(ObjectBin "gid",ObjectWord 4),(ObjectBin "group",ObjectBin "5"),(ObjectBin "hardlink_master",ObjectBool True),(ObjectBin "mode",ObjectWord 8),(ObjectBin "mtime",ObjectWord 3),(ObjectBin "path",ObjectBin "9"),(ObjectBin "size",ObjectWord 10),(ObjectBin "uid",ObjectWord 6),(ObjectBin "user",ObjectBin "7")]
		it "`fromObject . toObject == id` via ArchiveItem" $ do
			(fromObject $ toObject ai) `shouldBe` (Right ai :: Either String ArchiveItem)
