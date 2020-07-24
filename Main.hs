{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as M
import Data.MessagePack
import Data.MessagePack.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import RPC

main = do
	conn <- openRPC
	negotiate conn
	open conn "/home/l29ah/projects/hyborg/test"
	get conn repoManifest >>= print
	pure ()
