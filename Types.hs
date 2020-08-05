{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Map
import Data.MessagePack.Types
import Data.String.Class
import Data.Void
import Data.Word
import GHC.Generics

-- |32 bytes-long chunk identifier
newtype ID a = ID { fromID :: ByteString } deriving Eq
instance Show (ID a) where
	show (ID bs) = show $ B16.encode bs
instance ConvString (ID a) where
	toString (ID bs) = toString bs
	fromString = undefined

newtype Archive = Archive (Map ByteString Object) deriving (Show, Generic)
instance MessagePack Archive

newtype Manifest = Manifest (Map ByteString Object) deriving (Show, Generic)
instance MessagePack Manifest

newtype ArchiveItem = ArchiveItem Void

data CryptoMethod = CryptoMethod
	{ cmID :: Word8
	, cmDecrypt :: ByteString -> ByteString
	, cmHashID :: ByteString -> ID Void
	}
