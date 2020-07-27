{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main

import qualified Data.ByteString.Lazy as BL

import Chunker.BuzHash

megabyteByteString = BL.take 1000000 $ BL.iterate (+ 1) 0

main = defaultMain
	[ bgroup "buzhash"
		[ bench "1" $ nf (buzhash borgLookupTable) megabyteByteString
		]
	]
