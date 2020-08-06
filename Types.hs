{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}
module Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Map (Map)
import qualified Data.Map as M
import Data.MessagePack.Types
import Data.String.Class
import Data.Void
import Data.Word
import GHC.Generics

-- |32 bytes-long chunk identifier
newtype ID a = ID { fromID :: ByteString } deriving (Eq, Generic)
instance Show (ID a) where
	show (ID bs) = show $ B16.encode bs
instance ConvString (ID a) where
	toString (ID bs) = toString bs
	fromString = undefined
instance MessagePack (ID a) where
	fromObject (ObjectStr bs) = pure $ ID bs
	fromObject _ = fail "wrong messagepack type for ID"

newtype Archive = Archive (Map ByteString Object) deriving (Show, Generic)
instance MessagePack Archive

newtype Manifest = Manifest (Map ByteString Object) deriving (Show, Generic)
instance MessagePack Manifest

data ArchiveItem = ArchiveItem
	{ aiChunks :: [ID DataChunk]
	, aiATime :: Word64
	, aiCTime :: Word64
	, aiMTime :: Word64
	, aiGID :: Word
	, aiGroup :: ByteString
	, aiUID :: Word
	, aiUser :: ByteString
	, aiHardlinkMaster :: Bool
	, aiPath :: FilePath
	, aiSize :: Word64
	} deriving (Show)
instance MessagePack ArchiveItem where
	fromObject m@(ObjectMap _) = do
		ma <- fromObject m
		let	look :: (MessagePack a, MonadFail m) => ByteString -> m a
			look field = maybe (fail $ "no field " ++ toString field ++ " found") fromObject $ M.lookup field (ma :: Map ByteString Object) in
			ArchiveItem
				<$> look "chunks"
				<*> look "atime"
				<*> look "ctime"
				<*> look "mtime"
				<*> look "gid"
				<*> look "group"
				<*> look "uid"
				<*> look "user"
				<*> look "hardlink_master"
				<*> look "path"
				<*> look "size"
	fromObject _ = fail "wrong messagepack type for ArchiveItem"
	toObject ArchiveItem {..} = toObject $ M.fromList
		[ ("chunks" :: ByteString,	toObject aiChunks)
		, ("atime",			toObject aiATime)
		, ("ctime",			toObject aiCTime)
		, ("mtime",			toObject aiMTime)
		, ("gid",			toObject aiGID)
		, ("group",			toObject aiGroup)
		, ("uid",			toObject aiUID)
		, ("user",			toObject aiUser)
		, ("hardlink_master",		toObject aiHardlinkMaster)
		, ("path",			toObject aiPath)
		, ("size",			toObject aiSize)
		]

newtype DataChunk = DataChunk Void

data CryptoMethod = CryptoMethod
	{ cmID :: Word8
	, cmDecrypt :: ByteString -> ByteString
	, cmHashID :: ByteString -> ID Void
	}
