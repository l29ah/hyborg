module Cache where

import Conduit
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Serialization.Binary
import qualified Data.HashMap.Strict as HM
import Data.MessagePack
import Data.String.Class
import System.Environment.XDG.BaseDir
import System.IO

import Types

readCache :: ID Repository -> IO FileCache
readCache repoID = do
	cacheDir <- getUserCacheDir "borg"
	let filename = cacheDir ++ "/" ++ toString repoID ++ "/files"
	withFile filename ReadMode $ \handle -> do
		cacheTuples <- sourceToList $ sourceHandle handle .| conduitDecode .| CL.mapMaybe fromObject
		pure $ HM.fromList (cacheTuples :: [CacheTuple])
