{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, Strict #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Types
	( module Types.Generics
	, module Types
	) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.MessagePack.Types
import Data.String.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Void
import Data.Word
import qualified GHC.Generics as GHC
import Generics.SOP

import Types.Generics

bin2Str :: Object -> Object
bin2Str (ObjectBin b) = ObjectStr b
bin2Str (ObjectArray bs) = ObjectArray $ map bin2Str bs
bin2Str m@(ObjectMap _) = mapBin2Str m
bin2Str others = others

-- |borgbackup requires obsolete ObjectStr serialization for most of its byte arrays
mapBin2Str :: Object -> Object
mapBin2Str (ObjectMap kvlist) = ObjectMap $ map (\(k, v) -> (bin2Str k, bin2Str v)) kvlist
mapBin2Str x = x

-- |32 bytes-long chunk identifier
newtype ID a = ID { fromID :: ByteString } deriving (Eq, Hashable, GHC.Generic)
instance Show (ID a) where
	show (ID bs) = show $ B16.encode bs
instance ConvString (ID a) where
	toString (ID bs) = toString $ B16.encode bs
	fromString = ID . fst . B16.decode . fromString
instance MessagePack (ID a) where
	fromObject (ObjectStr bs) = pure $ ID bs
	fromObject _ = fail "wrong messagepack type for ID"

instance MessagePack UTCTime where
	-- FIXME we do not serialize %Q as borg is pissed at fractions longer than 6 decimal places
	toObject = toObject . T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
	fromObject x = (toFail "invalid time format" . parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" . T.unpack) =<< fromObject x

-- |Phantom data type to parametrize ID with
data Repository

data TAM = TAM
	{ hmac :: ByteString
	, salt :: ByteString
	, _type :: ByteString
	} deriving (Eq, Show, GHC.Generic)
instance Generic TAM
instance HasDatatypeInfo TAM
instance MessagePack TAM where
	toObject = mapBin2Str . gToObjectMap
	fromObject = gFromObjectMap
instance Default TAM where
	def = TAM
		(B.replicate 64 0)
		(B.replicate 64 0)
		"HKDF_HMAC_SHA512"

data Archive = Archive
	{ chunkerParams :: (Word, Word, Word, Word)
	, cmdline :: [ByteString]
	, comment :: ByteString
	, hostname :: ByteString
	, items :: [ID ArchiveItem]
	, name :: ByteString
	, tam :: TAM
	, time :: UTCTime
	, timeEnd :: UTCTime
	, username :: ByteString
	, version :: Word
	} deriving (Eq, Show, GHC.Generic)
instance Generic Archive
instance HasDatatypeInfo Archive
instance MessagePack Archive where
	toObject = mapBin2Str . gToObjectMap
	fromObject = gFromObjectMap

data DescribedArchive = DescribedArchive
	{ _id :: ID Archive
	, time :: UTCTime
	} deriving (Eq, Show, GHC.Generic)
instance Generic DescribedArchive
instance HasDatatypeInfo DescribedArchive
instance MessagePack DescribedArchive where
	toObject = mapBin2Str . gToObjectMap
	fromObject = gFromObjectMap

data Manifest = Manifest
	{ version :: Int
	, timestamp :: UTCTime
	, itemKeys :: [ByteString]
	, config :: Object
	, archives :: Map ByteString DescribedArchive
	, tam :: TAM
	} deriving (Eq, Show, GHC.Generic)
instance Generic Manifest
instance HasDatatypeInfo Manifest
instance MessagePack Manifest where
	toObject = mapBin2Str . gToObjectMap
	fromObject = gFromObjectMap

data DescribedChunk = DescribedChunk
	{ chunkID :: ID DataChunk
	, size :: Word64
	, compressedSize :: Word64
	} deriving (Eq, Show, GHC.Generic)
instance MessagePack DescribedChunk

data ArchiveItem = ArchiveItem
	{ atime :: Word64
	, ctime :: Word64
	, mtime :: Word64
	, gid :: Word32
	, group :: ByteString
	, uid :: Word32
	, user :: ByteString
	, mode :: Word32
	, hardlinkMaster :: Maybe Bool
	, path :: ByteString
	, size :: Word64
	, chunks :: Maybe [DescribedChunk]
	, source :: Maybe FilePath
	} deriving (Eq, Show, GHC.Generic)
instance Generic ArchiveItem
instance HasDatatypeInfo ArchiveItem
instance MessagePack ArchiveItem where
	fromObject obj = do
		m :: Map Text Object <- fromObject obj
		let	look :: (MonadFail m, MessagePack a) => Text -> m a
			look k = fromObject =<< (toFail ("no field " ++ T.unpack k ++ " present in MessagePack") $ M.lookup k m)
			mbLook :: (MonadFail m, MessagePack a) => Text -> m (Maybe a)
			mbLook k = pure $ fromObject =<< (M.lookup k m)
			justLook k d = pure $ fromMaybe d $ fromObject =<< (M.lookup k m)
		ArchiveItem
			<$> look "atime"
			<*> look "ctime"
			<*> look "mtime"
			<*> look "gid"
			<*> look "group"
			<*> look "uid"
			<*> look "user"
			<*> look "mode"
			<*> mbLook "hardlink_master"
			<*> look "path"
			<*> justLook "size" 0
			<*> mbLook "chunks"
			<*> mbLook "source"
	toObject ai = mapBin2Str $ toObject $ M.fromList $
		[ ("atime" :: Text, toObject ai.atime)
		, ("ctime", toObject ai.ctime)
		, ("mtime", toObject ai.mtime)
		, ("gid", toObject ai.gid)
		, ("uid", toObject ai.uid)
		, ("user", toObject ai.user)
		, ("group", toObject ai.group)
		, ("mode", toObject ai.mode)
		, ("hardlink_master", toObject ai.hardlinkMaster)
		, ("path", toObject ai.path)
		, ("size", toObject ai.size)
		] ++ maybe [] (\ch -> pure ("chunks", toObject ch)) ai.chunks
		++ maybe [] (\so -> pure ("source", toObject so)) ai.source

newtype DataChunk = DataChunk Void

data CryptoMethod = CryptoMethod
	{ cmID :: Word8
	, cmDecrypt :: ByteString -> ByteString
	, encrypt :: BL.ByteString -> BL.ByteString
	, hashID :: ByteString -> ID Void
	}

type CacheTuple = (ID FilePath, CacheEntry)

data CacheEntry = CacheEntry
	{ inode :: Word64
	, size :: Word64
	, mtime :: Word64
	, age :: Word
	, chunks :: [ID DataChunk]
	} deriving (Eq, Show, GHC.Generic)
instance MessagePack CacheEntry

type FileCache = HashMap (ID FilePath) CacheEntry

data BuzHashChunkerSettings = BuzHashChunkerSettings
	{ seed :: Word32
	, minExp :: Int
	, maxExp :: Int
	, maskBits :: Int
	, windowSize :: Int64
	} deriving (Eq, Show)
instance Default BuzHashChunkerSettings where
	def = BuzHashChunkerSettings 0 19 23 21 4095
