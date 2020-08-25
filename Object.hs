{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Object where

import qualified Control.Monad.Fail as Fail
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Coerce
import Data.List
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe
import Data.MessagePack
import Data.Word

import Compression
import RPC
import Types

plaintext :: CryptoMethod
plaintext = CryptoMethod
	{ cmID = 0x02
	, cmDecrypt = id
	, cmHashID = ID . SHA256.hash
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

listArchives :: Manifest -> [(ByteString, ID Archive, ByteString)]
listArchives manifest = map (\(name, ObjectMap info) -> (name, ID $ attr "id" info, attr "time" info)) $ M.toList manifest.archives
	where attr s = (\(ObjectStr s) -> s) . fromJust . lookup (ObjectStr s)

getArchive :: RPCHandle -> ID Archive -> IO Archive
getArchive conn aid = do
	adata <- get conn aid
	unpack $ BL.fromStrict $ fromJust $ decrypt adata

getArchives :: RPCHandle -> Manifest -> IO ()
getArchives conn manifest = do
	let archiveIDs = map (\(_, id, _) -> id) $ listArchives manifest
	mapM_ (\aid -> do
			archive <- getArchive conn aid
			print archive
			items :: [ID ArchiveItem] <- fmap (fmap ID) $ fromObject $ (coerce archive) ! "items"
			mapM_ (getArchiveItem conn) items
		) archiveIDs

archiveItems :: Archive -> [ID ArchiveItem]
archiveItems (Archive map) = fromMaybe [] $ do
	items <- M.lookup "items" map
	itemList <- case items of
		ObjectArray l -> Just l
		_ -> Nothing
	mapM fromObject itemList

getArchiveItem :: RPCHandle -> ID ArchiveItem -> IO ArchiveItem
getArchiveItem conn cid = do
	idata <- get conn cid
	unpack $ BL.fromStrict $ fromJust $ decrypt idata
