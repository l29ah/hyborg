{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DuplicateRecordFields, TypeApplications, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Map (Map)
import Data.MessagePack.Types
import Data.String.Class
import Data.Void
import Data.Word
import qualified GHC.Generics as GHC
import Generics.SOP

import Types.Generics

-- |32 bytes-long chunk identifier
newtype ID a = ID { fromID :: ByteString } deriving (Eq, GHC.Generic)
instance Show (ID a) where
	show (ID bs) = show $ B16.encode bs
instance ConvString (ID a) where
	toString (ID bs) = toString $ B16.encode bs
	fromString = undefined
instance MessagePack (ID a) where
	fromObject (ObjectStr bs) = pure $ ID bs
	fromObject _ = fail "wrong messagepack type for ID"

newtype Archive = Archive (Map ByteString Object) deriving (Eq, Show, GHC.Generic)
instance MessagePack Archive

data Manifest = Manifest
	{ version :: Int
	, timestamp :: ByteString
	, itemKeys :: [ByteString]
	, config :: Object
	, archives :: Map ByteString Object
	, tam :: Object
	} deriving (Eq, Show, GHC.Generic)
instance Generic Manifest
instance HasDatatypeInfo Manifest
instance MessagePack Manifest where
	toObject = gToObjectMap
	fromObject = gFromObjectMap

data ArchiveItem = ArchiveItem
	{ chunks :: [ID DataChunk]
	, atime :: Word64
	, ctime :: Word64
	, mtime :: Word64
	, gid :: Word
	, group :: ByteString
	, uid :: Word
	, user :: ByteString
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
	, cmHashID :: ByteString -> ID Void
	}
