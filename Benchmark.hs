{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main

import Control.DeepSeq
import Control.Parallel.Strategies
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default

import Chunker.BuzHash
import qualified Chunker.Fixed as Fix
import Compression

threadify threads computation = parMap rdeepseq computation . replicate threads

genByteStringS len = B.pack $ take len $ iterate (+ 1) 0
genByteString = BL.fromStrict . genByteStringS

byteString1M	= genByteString 1000000
byteString10M	= genByteString 10000000
byteString10MS	= genByteStringS 10000000

main = do
	putStrLn "preheating the CPU"
	deepseq ((map $ buzhash borgLookupTable) $ replicate 40 byteString10M) $ pure ()
	defaultMain
		[ bgroup "buzhash"
			[ bench "5x10MB" $ nf (map $ buzhash borgLookupTable) $ replicate 5 byteString10M
			, bench "5x10MB via 5 threads" $ nf (threadify 5 $ buzhash borgLookupTable) byteString10M
			]
		, bgroup "buzhashUpdate"
			[ bench "" $ nf (buzhashUpdate borgLookupTable 0x12345678 0x90 0x12) 0x3
			]
		, bgroup "chunkify"
			[ bench "default settings 1MB" $ nf (chunkify def) byteString1M
			, bench "default settings 10MB" $ nf (chunkify def) byteString10M
			, bench "default settings 5x10MB via 5 threads" $ nf (threadify 5 $ length . chunkify def) byteString10M
			]
		, bgroup "fixed chunkify"
			[ bench "1MiB chunks, 10MB" $ nf (Fix.chunkify (2^20)) byteString10M
			, bench "1MiB chunks, 5x10MB via 5 threads" $ nf (threadify 5 $ length . Fix.chunkify (2^20)) byteString10M
			]
		, bgroup "compression"
			[ bench "LZ4 compress-decompress-verify, 10MB" $ nf (\s -> byteString10MS == (decompress $ BL.toStrict $ compress s)) byteString10MS
			, bench "LZ4 compress-decompress-verify, 5x10MB via 5 threads" $ nf (threadify 5 (\s -> byteString10MS == (decompress $ BL.toStrict $ compress s))) byteString10MS
			]
		]
