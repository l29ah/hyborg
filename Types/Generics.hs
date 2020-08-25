{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, FlexibleContexts, TypeOperators, DataKinds, PartialTypeSignatures, TypeApplications, FlexibleInstances #-}
module Types.Generics where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.MessagePack
import Data.SOP.NP
import Generics.SOP
import Text.Casing

-- TODO class declaration to reduce tToObjectMap/gFromObjectMap usage boilerplate

gToObjectMap :: forall a xs. (IsProductType a xs, HasDatatypeInfo a, All MessagePack xs) => a -> Object
gToObjectMap x = toObject $ M.fromList $ collapse_NP $ hcliftA2 (Proxy :: Proxy MessagePack) toTuple (getFields $ Proxy @a) dat where
	dat = (\(SOP (Z fields)) -> fields) $ from x

toTuple (FieldInfo fn) (I dat) = K (B.pack $ toSnakeName fn, toObject dat)

toSnakeName :: String -> String
toSnakeName field = let prefixLength = length $ fst $ break isUpper field in
	toQuietSnake $ fromHumps $ drop prefixLength field

-- |List the field names of a product type
getFields :: forall a xs. (IsProductType a xs, HasDatatypeInfo a) => Proxy a -> NP FieldInfo xs
getFields proxy = (\(ADT _ _ (Record _ fields :* Nil) _) -> fields) $ datatypeInfo proxy

toFail :: MonadFail m => String -> Maybe a -> m a
toFail str = maybe (fail str) pure

gFromObjectMap :: forall a xs m. (IsProductType a xs, HasDatatypeInfo a, All MessagePack xs, MonadFail m) => Object -> m a
gFromObjectMap x = do
	myMap :: Map ByteString Object <- fromObject x
	contents <- hsequence $ hcliftA (Proxy @MessagePack) (\(FieldInfo fn :: FieldInfo z) -> do
			content <- toFail ("no field " ++ fn ++ " present in MessagePack") $ M.lookup (B.pack $ toSnakeName fn) myMap
			fromObject content :: m z
		) $ getFields $ Proxy @a
	pure $ productTypeTo contents
