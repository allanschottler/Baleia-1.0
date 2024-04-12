{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Baleia.Meta where

import Language.Haskell.TH

import Data.Typeable (Typeable, typeOf)
import Data.Data (Data, gfoldl, gmapQi, ConstrRep(AlgConstr),
                  toConstr, constrRep, constrFields, tyconUQname )
import Data.Generics (extT, extQ)   
import Data.List (elemIndex)
import Control.Arrow ((&&&))         

introspect :: Name -> Q Exp
introspect n = do 
            rfs <- fmap getRecordFields $ reify n 
            litE . stringL $ show rfs

getRecordFields :: Info -> [(String, String)]
getRecordFields (TyConI (DataD _ _ _ _ cons _)) = concatMap getRF' cons
getRecordFields _ = []

getRF' :: Con -> [(String, String)]
getRF' (RecC name fields) = map getFieldInfo fields
getRF' _ = []

getFieldInfo :: (Name, Strict, Type) -> (String, String)
getFieldInfo (name, _, ty) = (nameBase name, tyconUQname $ show ty)

---------------------------------------------------------------------------------------

data Ti a = Ti Int a

gmapTi :: Data a => Int -> (forall b. Data b => b -> b) -> a -> a
gmapTi i f x = case gfoldl k z x of { Ti _ a -> a }
  where
    k :: Data d => Ti (d->b) -> d -> Ti b
    k (Ti i' c) a = Ti (i'+1) (if i==i' then c (f a) else c a)
    z :: g -> Ti g
    z = Ti 0

---------------------------------------------------------------------------------------

fieldNames :: (Data r) => r -> [String]
fieldNames rec =
  case (constrRep &&& constrFields) $ toConstr rec of
    (AlgConstr _, fs) | not $ null fs -> fs
    otherwise                         -> error "Not a record type"

fieldIndex :: (Data r) => String -> r -> Int
fieldIndex fieldName rec =
  case fieldName `elemIndex` fieldNames rec of
    Just i  -> i
    Nothing -> error $ "No such field: " ++ fieldName

modifyField :: (Data r, Typeable v) => String -> (v -> v) -> r -> r
modifyField fieldName m rec = gmapTi i (e `extT` m) rec
  where
    i = fieldName `fieldIndex` rec
    e x = error $ "Type mismatch: " ++ fieldName ++
                             " :: " ++ (show . typeOf $ x) ++
                           ", not " ++ (show . typeOf $ m undefined)

setField :: (Data r, Typeable v) => String -> v -> r -> r
setField fieldName value = modifyField fieldName (const value)

getField :: (Data r, Typeable v) => String -> r -> v
getField fieldName rec = gmapQi i (e `extQ` id) rec
  where
    i = fieldName `fieldIndex` rec
    e x = error $ "Type mismatch: " ++ fieldName ++
                             " :: " ++ (show . typeOf $ x) ++
                           ", not " ++ (show . typeOf $ e undefined)