{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main

import Control.Parallel.Strategies
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Chunker.BuzHash

threadify threads computation = parMap rdeepseq computation . replicate threads

genByteString len = BL.fromStrict $ B.pack $ take len $ iterate (+ 1) 0

byteString1M	= genByteString 1000000
byteString10M	= genByteString 10000000

main = defaultMain
	[ bgroup "buzhash"
		[ bench "10MB" $ nf (buzhash borgLookupTable) byteString10M
		, bench "5x10MB via 5 threads" $ nf (threadify 5 $ buzhash borgLookupTable) byteString10M
		]
	, bgroup "buzhashUpdate"
		[ bench "" $ nf (buzhashUpdate borgLookupTable 0x12345678 0x90 0x12) 0x3
		]
	, bgroup "chunkify"
		[ bench "default settings 1MB" $ nf (chunkify 0 19 23 21 4095) byteString1M
		, bench "default settings 10MB" $ nf (chunkify 0 19 23 21 4095) byteString10M
		, bench "default settings 5x10MB via 5 threads" $ nf (threadify 5 $ length . chunkify 0 19 23 21 4095) byteString10M
		]
	]
