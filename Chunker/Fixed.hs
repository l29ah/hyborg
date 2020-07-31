module Chunker.Fixed where

import qualified Data.ByteString.Lazy as BL
import Data.Int

chunkify :: Int64 -> BL.ByteString -> [BL.ByteString]
chunkify len bs
	| BL.null bs = []
	| otherwise =
		let (l, r) = BL.splitAt len bs
		in l : chunkify len r
