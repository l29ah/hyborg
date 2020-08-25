{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}
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
	toString (ID bs) = toString bs
	fromString = undefined
instance MessagePack (ID a) where
	fromObject (ObjectStr bs) = pure $ ID bs
	fromObject _ = fail "wrong messagepack type for ID"

newtype Archive = Archive (Map ByteString Object) deriving (Show, GHC.Generic)
instance MessagePack Archive

newtype Manifest = Manifest (Map ByteString Object) deriving (Show, GHC.Generic)
instance MessagePack Manifest

data ArchiveItem = ArchiveItem
	{ aiChunks :: [ID DataChunk]
	, aiAtime :: Word64
	, aiCtime :: Word64
	, aiMtime :: Word64
	, aiGID :: Word
	, aiGroup :: ByteString
	, aiUID :: Word
	, aiUser :: ByteString
	, aiHardlinkMaster :: Bool
	, aiPath :: ByteString
	, aiSize :: Word64
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
