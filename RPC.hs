{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, Strict #-}
module RPC
	( RPCHandle
	, repoManifest
	, openRPC
	, openPseudoRPC
	, open
	, get
	, put
	, commit
	) where

import Conduit
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Short (ShortByteString, toShort, fromShort)
import qualified Data.ByteString.Short as BS
import Data.Conduit.Process
import Data.Conduit.Serialization.Binary
import Data.Conduit.TQueue
import Data.Functor
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Data.MessagePack
import Data.Text (Text)
import GHC.Generics
import System.Environment

import Orphans
import Types

repoManifest :: ID Manifest
repoManifest = ID $ BS.pack $ replicate 32 0

data RPCVersion = RPCVersion Word Word Word Int deriving Generic
instance MessagePack RPCVersion

data RPCOldRequest a = RPCOldRequest
	{ reqStuff0 :: Word
	, reqID :: Word
	, reqMethod :: Text
	, reqArguments :: [Map Text a]
	} deriving Generic
instance (MessagePack a) => MessagePack (RPCOldRequest a)

data RPCOldResponse a = RPCOldResponse
	{ resStuff0 :: Word
	, resID :: Word
	, resStuff2 :: ()
	, resResult :: Map Text a
	} deriving (Show, Generic)
instance (MessagePack a) => MessagePack (RPCOldResponse a)

data RPCHandle = RPCHandle
	{ toBorg :: TBQueue Object
	, fromBorg :: Maybe (TBQueue Object)
	}

openRPC :: IO RPCHandle
openRPC = do
	userRsh <- lookupEnv "BORG_RSH"
	let rsh = fromMaybe "ssh" userRsh	-- as in borg
	let queueLength = 20
	stdin <- newTBQueueIO queueLength
	stdout <- newTBQueueIO queueLength
	let cp = (shell rsh) { std_err = Inherit }
	let outputConsumer = conduitDecode .| sinkTBQueue stdout
	let inputProducer =  sourceTBQueue stdin .| mapC (BL.toStrict . pack)
	forkIO $ void $ sourceProcessWithIOStreams cp inputProducer outputConsumer
	let rh = RPCHandle stdin $ Just stdout
	negotiate rh
	pure rh

openPseudoRPC :: IO RPCHandle
openPseudoRPC = do
	let queueLength = 20
	stdin <- newTBQueueIO queueLength
	forkIO $ void $ runConduit $ sourceTBQueue stdin .| mapC (BL.toStrict . pack) .| sinkNull
	pure $ RPCHandle stdin Nothing

sendOldRequest :: (MessagePack a) => RPCHandle -> Text -> Map Text a -> IO ()
sendOldRequest conn method args = atomically $ writeTBQueue (toBorg conn) $ toObject $ RPCOldRequest 1 0 method [args]

receiveOldResponse :: (MessagePack a) => RPCHandle -> IO (RPCOldResponse a)
receiveOldResponse (RPCHandle _ (Just fromBorg)) = do
	resp <- atomically $ readTBQueue fromBorg
	fromObject resp
receiveOldResponse (RPCHandle _ Nothing) = undefined

sendRequest :: (MessagePack a) => RPCHandle -> ByteString -> Map Text a -> IO ()
sendRequest conn method args = atomically $ writeTBQueue (toBorg conn) $ ObjectMap
	[ (ObjectStr "i", ObjectWord id)
	, (ObjectStr "m", ObjectStr method)
	, (ObjectStr "a", toObject args)
	] where id = 0

receiveResponse :: RPCHandle -> IO ByteString
receiveResponse (RPCHandle _ (Just fromBorg)) = do
	resp <- atomically $ readTBQueue fromBorg
	respMap :: Map ByteString Object <- fromObject resp
	let result = respMap ! ("r" :: ByteString)
	pure $ case result of
		~(ObjectStr res) -> res
		_ -> error $ "Unexpected response received from the remote borg: " ++ show respMap
receiveResponse (RPCHandle _ Nothing) = pure $ B.pack $ replicate 32 0

negotiate :: RPCHandle -> IO ()
negotiate conn = do
	sendOldRequest conn "negotiate" $ M.fromList [("client_version", RPCVersion 1 1 13 (-1))]
	resp <- (receiveOldResponse conn) :: IO (RPCOldResponse Object)
	-- TODO check that the server supports our features
	pure ()

open :: RPCHandle -> ByteString -> Bool -> IO (ID Repository)
open conn path isWrite = do
	sendRequest conn "open" $ M.fromList
		[ ("path", ObjectStr path)
		, ("lock_wait", ObjectWord $ if isWrite then 1 else 0)
		, ("lock", ObjectBool isWrite)
		, ("exclusive", ObjectBool isWrite)
		]
	resp <- receiveResponse conn
	pure $ ID $ toShort resp

get :: RPCHandle -> ID a -> IO ByteString
get conn id = assert (B.length (fromShort $ fromID id) == 32) $ do
	sendRequest conn "get" $ M.fromList [("id", ObjectStr $ fromShort $ fromID id)]
	receiveResponse conn

put :: RPCHandle -> ID a -> ByteString -> IO ()
put conn id dat = assert (B.length (fromShort $ fromID id) == 32) $ do
	sendRequest conn "put" $ M.fromList
		[ ("id", ObjectStr $ fromShort $ fromID id)
		, ("data", ObjectStr dat)
		, ("wait", ObjectBool True)
		]
	void $ receiveResponse conn

commit :: RPCHandle -> IO ()
commit conn = do
	sendRequest conn "commit" $ M.fromList
		[ ("save_space", ObjectBool False)
		]
	void $ receiveResponse conn
