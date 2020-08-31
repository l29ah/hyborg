{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Object where

import qualified Control.Monad.Fail as Fail
import Crypto.Hash.Algorithms
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.KDF.HKDF as HKDF
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Random.MWC
import Data.Coerce
import Data.Default
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.MessagePack
import Data.Time
import Data.Word

import Compression
import RPC
import Types

import Debug.Trace

plaintext :: CryptoMethod
plaintext = CryptoMethod
	{ cmID = 0x02
	, cmDecrypt = id
	, encrypt = id
	, hashID = ID . SHA256.hash
	}

methods = [plaintext]

getMethod :: (MonadFail m) => Word8 -> m CryptoMethod
getMethod mid = toFail ("no such CryptoMethod: " ++ show mid) $ find (\m -> mid == cmID m) methods

decrypt :: (MonadFail m) => ByteString -> m ByteString
decrypt dat =
	let (encryptionType, encryptedData) = B.splitAt 1 dat in
	-- src/borg/crypto/key.py
	do
		method <- getMethod $ B.head encryptionType
		pure $ decompress $ method.cmDecrypt encryptedData

encrypt :: CryptoMethod -> BL.ByteString -> BL.ByteString
encrypt method dat = BL.concat [BL.pack [method.cmID], method.encrypt $ compress $ BL.toStrict dat]

readManifest :: (Fail.MonadFail m) => ByteString -> m Manifest
readManifest serialized = do
	decrypted <- decrypt serialized
	obj <- unpack $ BL.fromStrict decrypted
	fromObject $ traceShowId obj

listArchives :: Manifest -> [(ByteString, ID Archive, UTCTime)]
listArchives manifest = map (\(name, describedArchive) -> (name, describedArchive._id, describedArchive.time)) $ M.toList manifest.archives

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
			mapM_ (getArchiveItem conn) archive.items
		) archiveIDs

getArchiveItem :: RPCHandle -> ID ArchiveItem -> IO ArchiveItem
getArchiveItem conn cid = do
	idata <- get conn cid
	unpack $ BL.fromStrict $ fromJust $ decrypt idata

makeKey salt context = salt <> context

hMAC key dat = BA.convert $ HMAC.hmacGetDigest $ HMAC.hmac @ByteString @ByteString @SHA512 key dat

addTAMm :: Manifest -> IO Manifest
addTAMm m = do
	salt <- random 64
	let preTAM = def{salt = salt}
	let preM = m{tam = preTAM}
	let packedPreM = BL.toStrict $ pack preM
	let context = "manifest"
	let key = makeKey preTAM.salt context
	let hash = hMAC key packedPreM
	pure preM{tam = preTAM{hmac = hash}}

addTAMa :: Archive -> IO Archive
addTAMa archive = do
	salt <- random 64
	let preTAM = def{salt = salt}
	let preArchive = archive{tam = preTAM}
	let packedPreArchive = BL.toStrict $ pack preArchive
	let context = "manifest"
	-- TODO let ikm = self.id_key + self.enc_key + self.enc_hmac_key,
	-- let info = "borg-metadata-authentication-manifest"
	-- let outputLength = 64
	-- let key = hkdf ikm salt info outputLength
	let key = makeKey preTAM.salt context
	let hash = hMAC key packedPreArchive
	pure preArchive{tam = preTAM{hmac = hash}}

hkdf :: ByteString -> ByteString -> ByteString -> Int -> ByteString
hkdf ikm salt info len = HKDF.expand (HKDF.extract @SHA512 salt ikm) info len

serialize :: MessagePack object => CryptoMethod -> object -> (ByteString, ID object)
serialize encryption o = let serializedObject = BL.toStrict $ Object.encrypt encryption $ pack $ traceShowId $ toObject o
	in (serializedObject, coerce $ encryption.hashID serializedObject)
