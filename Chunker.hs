module Chunker where

import qualified Data.ByteString.Lazy as BL
import System.IO
import System.Posix.IO
import System.Posix.Types

import qualified Chunker.BuzHash as BH
import Types

chunkifyFile :: BuzHashChunkerSettings -> Fd -> IO [BL.ByteString]
chunkifyFile settings fd = do
	newFd <- dup fd	-- FIXME kludge to avoid hGetContents closing our handle
	hdl <- fdToHandle newFd
	hSetBinaryMode hdl True
	dat <- BL.hGetContents hdl
	pure $ BH.chunkify settings dat
