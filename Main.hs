{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

import Data.Binary (decode)
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.MessagePack

import Object
import RPC

main = do
	conn <- openRPC
	negotiate conn
	open conn "/home/l29ah/projects/hyborg/test"
	manifest <- get conn repoManifest
	print $ runGet getManifest $ BL.fromStrict manifest
	pure ()
