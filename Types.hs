{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, Strict #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Int
import Data.Map (Map)
import Data.MessagePack.Types
import Data.String.Class
import Data.Void
import Data.Word
import qualified GHC.Generics as GHC
import Generics.SOP

import Types.Generics

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
	toObject = gToObjectMap
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
	, time :: ByteString
	, timeEnd :: ByteString
	, username :: ByteString
	, version :: Word
	} deriving (Eq, Show, GHC.Generic)
instance Generic Archive
instance HasDatatypeInfo Archive
instance MessagePack Archive where
	toObject = gToObjectMap
	fromObject = gFromObjectMap

data DescribedArchive = DescribedArchive
	{ _id :: ID Archive
	, time :: ByteString
	} deriving (Eq, Show, GHC.Generic)
instance Generic DescribedArchive
instance HasDatatypeInfo DescribedArchive
instance MessagePack DescribedArchive where
	toObject = gToObjectMap
	fromObject = gFromObjectMap

data Manifest = Manifest
	{ version :: Int
	, timestamp :: ByteString
	, itemKeys :: [ByteString]
	, config :: Object
	, archives :: Map ByteString DescribedArchive
	, tam :: TAM
	} deriving (Eq, Show, GHC.Generic)
instance Generic Manifest
instance HasDatatypeInfo Manifest
instance MessagePack Manifest where
	toObject = gToObjectMap
	fromObject = gFromObjectMap

data DescribedChunk = DescribedChunk
	{ chunkID :: ID DataChunk
	, size :: Word64
	, compressedSize :: Word64
	} deriving (Eq, Show, GHC.Generic)
instance MessagePack DescribedChunk

data ArchiveItem = ArchiveItem
	{ chunks :: [DescribedChunk]
	, atime :: Word64
	, ctime :: Word64
	, mtime :: Word64
	, gid :: Word32
	, group :: ByteString
	, uid :: Word32
	, user :: ByteString
	, mode :: Word32
	, hardlinkMaster :: Bool
	, path :: ByteString
	, size :: Word64
	} deriving (Eq, Show, GHC.Generic)
instance Generic ArchiveItem
instance HasDatatypeInfo ArchiveItem
instance MessagePack ArchiveItem where
	fromObject = gFromObjectMap
	toObject = gToObjectMap

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
