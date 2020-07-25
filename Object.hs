module Object where

import Data.Binary (decode)
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.MessagePack

import RPC

getManifest :: Get [Object]
getManifest = do
	empty <- isEmpty
	if empty then return [] else do
		obj <- getObject
		objs <- getManifest
		pure $ obj:objs

-- manifest of an empty unencrypted repo:
-- [ObjectWord 2,ObjectWord 1,ObjectWord 0,ObjectInt (-16),ObjectWord 29,ObjectMap [(ObjectStr "archives",ObjectMap []),(ObjectStr "config",ObjectMap []),(ObjectStr "item_keys",ObjectArray [ObjectStr "acl_access",ObjectStr "\v\NUL\128default\172",ObjectWord 12,ObjectWord 0,ObjectArray [],ObjectWord 101,ObjectWord 120,ObjectWord 116,ObjectWord 101,ObjectWord 110,ObjectWord 100,ObjectWord 101,ObjectWord 100,ObjectStr "\r\NUL\240\SOHnfs4",ObjectStr "atime",ObjectStr "birth\n\NUL\242\STX",ObjectStr "bsdflags",ObjectStr "chunks",ObjectStr "\a\NUL\160_healthy\165c%",ObjectWord 0,ObjectInt (-16),ObjectWord 18,ObjectStr "gid"]),(ObjectStr "group",ObjectStr "hardlink_master"),(ObjectStr "mode",ObjectStr "m%\NUL\240\200"),(ObjectStr "part",ObjectStr "path")],ObjectStr "rdev",ObjectStr "size",ObjectStr "source",ObjectStr "uid",ObjectStr "user",ObjectStr "xattrs",ObjectStr "tam",ObjectMap [(ObjectStr "hmac",ObjectStr "(0y\131xy\EM\255\208\247\232UX\f\233\DC1_A\168\t\207\188M\154\v\191u\FSn7\CAN,\157\225\206V\226wG\184a\213\184},\ESCo\190N\251\190\185\177\217V6\NAK\SI\207\133\195(Y\210"),(ObjectStr "salt",ObjectStr "\164\198$\v\249\145s7\178\182\SYN\161\&9\221&\185g\254\201X\237\rV\136\149A\156v=X\172Y\215\132b\179\171:\185\128\204\194\237\135\&0c\207\213\187\189G.\tv\193\r\EOT\244\"\243\&0\197\ENQ@"),(ObjectStr "type",ObjectStr "HKDF_HMAC_SHA512")],ObjectStr "\219\NUL\240\SUBstamp",ObjectStr "2020-07-23T15:36:13.546651",ObjectStr "version",ObjectWord 1]
