{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
module Types.Generics where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.MessagePack
import Data.SOP.NP
import qualified GHC.Generics as GHC
import Generics.SOP
import Text.Casing

--class (Generic a, HasDatatypeInfo a, All2 MessagePack (Code a)) => GMessagePackMap a where
--	gToObjectMap :: a -> Object
--	default gToObjectMap :: a -> Object
--	--gFromObjectMap :: MonadFail m => Object -> m a

gToObjectMap :: forall a xs. (IsProductType a xs, HasDatatypeInfo a, All MessagePack xs) => a -> Object
gToObjectMap x = toObject $ M.fromList $ collapse_NP $ hcliftA2 (Proxy :: Proxy MessagePack) toTuple recs dat where
	recs = (\(ADT _ _ (Record _ fields :* Nil) _) -> fields) $ datatypeInfo (Proxy :: Proxy a)
	dat = (\(SOP (Z fields)) -> fields) $ from x

toTuple (FieldInfo fn) (I dat) = K (B.pack $ toSnakeName fn, toObject dat)

toSnakeName :: String -> String
toSnakeName field = let prefixLength = length $ fst $ break isUpper field in
	toQuietSnake $ fromHumps $ drop prefixLength field

--gFromObjectMap :: forall a xs m. (IsProductType a xs, HasDatatypeInfo a, All MessagePack xs, MonadFail m) => Object -> m a
--gFromObjectMap x = do
--	myMap :: Map ByteString Object <- fromObject x
--	let recs = (\(ADT _ _ (Record _ fields :* Nil) _) -> fields) $ datatypeInfo (Proxy :: Proxy a)
--	--let dat = (\(SOP (Z fields)) -> fields) $ from x
--	--let sop =
--	hcollapse $ hcliftA2 (Proxy :: Proxy MessagePack) fromMap recs $ M.toList myMap
--	--pure undefined
--
--fromMap :: MonadFail m => FieldInfo a -> Map ByteString Object -> m K
--fromMap (FieldInfo fn) m = fromJust $ M.lookup (B.pack fn) m
