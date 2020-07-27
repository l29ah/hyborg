{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main

import qualified Data.ByteString.Lazy as BL

import Chunker.BuzHash

byteString1M	= BL.take 1000000 $ BL.iterate (+ 1) 0
byteString10M	= BL.take 10000000 $ BL.iterate (+ 1) 0

main = defaultMain
	[ bgroup "buzhash"
		[ bench "1" $ nf (buzhash borgLookupTable) byteString10M
		]
	, bgroup "chunkify"
		[ bench "default settings 1MB" $ nf (chunkify 0 19 23 21 4095) byteString1M
		, bench "default settings 10MB" $ nf (chunkify 0 19 23 21 4095) byteString10M
		]
	]
