{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Types.GenericsSpec (spec) where

import Data.Default
import Data.MessagePack
import Test.Hspec

import Types

instance MonadFail (Either String) where
	fail = Left

spec :: Spec
spec = do
	describe "gToObjectMap" $ do
		it "`fromObject . toObject == id` via TAM" $ do
			(fromObject $ toObject (def :: TAM)) `shouldBe` (Right def :: Either String TAM)
