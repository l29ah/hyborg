{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Object where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Map (Map, (!))
import Data.Maybe
import Data.MessagePack
import Data.Word

import RPC
import Compression

data CryptoMethod = CryptoMethod
	{ cmID :: Word8
	, cmDecrypt :: ByteString -> ByteString
	, cmHashID :: ByteString -> ChunkID
	}

plaintext :: CryptoMethod
plaintext = CryptoMethod
	{ cmID = 0x02
	, cmDecrypt = id
	, cmHashID = SHA256.hash
	}

methods = [plaintext]

getMethod :: Word8 -> Maybe CryptoMethod
getMethod mid = find (\m -> mid == cmID m) methods

decrypt :: ByteString -> Maybe ByteString
decrypt dat = 
	let (encryptionType, encryptedData) = B.splitAt 1 dat in
	-- src/borg/crypto/key.py
	do
		method <- getMethod $ B.head encryptionType
		pure $ decompress $ cmDecrypt method encryptedData

readManifest :: (MonadFail m) => ByteString -> m (Map ByteString Object)
readManifest = unpack . BL.fromStrict . fromJust . decrypt

type Archive = Map ByteString Object

listArchives :: Map ByteString Object -> [(ByteString, ChunkID)]
listArchives manifest = let ObjectMap archives = manifest ! "archives" in
	map (\(ObjectStr name, ObjectMap info) -> (name, (\(ObjectStr s) -> s) $ fromJust $ lookup (ObjectStr "id") info)) archives

getArchives :: RPCHandle -> Map ByteString Object -> IO ()
getArchives conn manifest = do
	let archiveIDs = map snd $ listArchives manifest
	mapM_ (\aid -> do
			adata <- get conn aid
			archive :: Archive <- unpack $ BL.fromStrict $ fromJust $ decrypt adata
			print archive
			items :: [ChunkID] <- fromObject $ archive ! "items"
			mapM_ (getArchiveItem conn) items
		) archiveIDs
	
getArchiveItem :: RPCHandle -> ChunkID -> IO ()
getArchiveItem conn cid = do
	idata <- get conn cid
	item :: Object <- unpack $ BL.fromStrict $ fromJust $ decrypt idata
	print item
