{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Object where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Map (Map, (!))
import qualified Control.Monad.Fail as Fail
import Data.Maybe
import Data.MessagePack
import Data.Word

import RPC
import Compression

type Archive = Map ByteString Object
type Manifest = Map ByteString Object

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

readManifest :: (Fail.MonadFail m) => ByteString -> m Manifest
readManifest = unpack . BL.fromStrict . fromJust . decrypt

listArchives :: Manifest -> [(ByteString, ChunkID, ByteString)]
listArchives manifest = let ObjectMap archives = manifest ! "archives" in
	map (\(ObjectStr name, ObjectMap info) -> (name, attr "id" info, attr "time" info)) archives
	where attr s = (\(ObjectStr s) -> s) . fromJust . lookup (ObjectStr s)

getArchives :: RPCHandle -> Manifest -> IO ()
getArchives conn manifest = do
	let archiveIDs = map (\(_, id, _) -> id) $ listArchives manifest
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
