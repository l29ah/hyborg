{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module RPC where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Data.Conduit.Serialization.Binary
import Data.Functor
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Data.MessagePack
import Data.MessagePack.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import GHC.Generics
import System.Environment
import System.Process

type ObjectID = ByteString

repoManifest :: ObjectID
repoManifest = B.pack $ replicate 32 0

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

type RPCHandle = (Chan Object, Chan Object)

openRPC :: IO RPCHandle
openRPC = do
	userRsh <- lookupEnv "BORG_RSH"
	--let rsh = fromMaybe "ssh" userRsh
	let rsh = fromJust userRsh
	print rsh
	stdin <- newChan
	stdout <- newChan
	stdinContents <- getChanContents stdin
	let cp = (shell rsh) { std_err = Inherit }
	let outputConsumer = conduitDecode .| (CL.mapM_ $ writeChan stdout)
	let inputProducer = CL.sourceList $ map (BL.toStrict . pack) $ stdinContents
	forkIO $ void $ sourceProcessWithStreams cp inputProducer outputConsumer (pure ())
	pure (stdin, stdout)


sendOldRequest :: (MessagePack a) => RPCHandle -> Text -> Map Text a -> IO ()
sendOldRequest conn method args = writeChan (fst conn) $ toObject $ RPCOldRequest 1 0 method [args]

receiveOldResponse :: (MessagePack a) => RPCHandle -> IO (RPCOldResponse a)
receiveOldResponse conn = do
	resp <- readChan $ snd conn
	fromObject resp

sendRequest :: (MessagePack a) => RPCHandle -> ByteString -> Map Text a -> IO ()
sendRequest conn method args = writeChan (fst conn) $ ObjectMap
	[ (ObjectStr "i", ObjectWord id)
	, (ObjectStr "m", ObjectStr method)
	, (ObjectStr "a", toObject args)
	] where id = 0

receiveResponse :: RPCHandle -> IO ByteString
receiveResponse conn = do
	resp <- readChan $ snd conn
	respMap :: Map ByteString Object <- fromObject resp
	let ObjectStr result = respMap ! ("r" :: ByteString)
	pure result

negotiate :: RPCHandle -> IO ()
negotiate conn = do
	sendOldRequest conn "negotiate" $ M.fromList [("client_version", RPCVersion 1 1 13 (-1))]
	resp <- (receiveOldResponse conn) :: IO (RPCOldResponse Object)
	print resp

open :: RPCHandle -> Text -> IO ()
open conn path = do
	sendRequest conn "open" $ M.fromList
		[ ("path", path)
		]
	-- apparently it produces some 32byte ID
	print =<< receiveResponse conn

get :: RPCHandle -> ObjectID -> IO ByteString
get conn id = assert (B.length id == 32) $ do
	sendRequest conn "get" $ M.fromList [("id", ObjectStr id)]
	receiveResponse conn
