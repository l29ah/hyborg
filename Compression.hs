module Compression
	( decompress
	) where

import qualified Codec.Lz4 as LZ4
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

decompress :: ByteString -> ByteString
decompress d = let (compressionType, compressedData) = B.splitAt 2 d in
	-- src/borg/compress.pyx
	case B.unpack compressionType of
		[1, 0] -> lz4Decompress compressedData

-- FIXME sane allocation logic
lz4Decompress :: ByteString -> ByteString
lz4Decompress d = LZ4.decompressBlockSz d $ 2^24
