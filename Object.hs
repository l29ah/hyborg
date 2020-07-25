module Object where

import Data.Binary (decode)
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import Data.MessagePack

import RPC
import Compression

decrypt :: ByteString -> ByteString
decrypt ciphertext = 
	let (encryptionType, encryptedData) = B.splitAt 1 ciphertext in
	-- src/borg/crypto/key.py
	case B.head encryptionType of
		0x02 -> decompress encryptedData

readManifest :: (MonadFail m) => ByteString -> m (Map ByteString Object)
readManifest = unpack . BL.fromStrict . decrypt
