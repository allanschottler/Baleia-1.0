{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Baleia.Meta where

import Language.Haskell.TH

import Data.Typeable (Typeable, typeOf)
import Data.Data (Data, gfoldl, gmapQi, gmapQ, ConstrRep(AlgConstr),
                  toConstr, fromConstr, constrRep, constrFields, tyconUQname )
import Data.Generics (extT, extQ)   
import Data.Typeable (cast, Typeable, typeOf)
import Data.List (elemIndex)
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Language.Haskell.TH.Syntax (Name)
import Data.Maybe (Maybe(..)) -- Ensure Maybe and Just are in scope
import Control.Arrow ((&&&)) 
import Control        

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

-- | Get the list of record field names.
-- fieldNames :: Data r => r -> [String]
-- fieldNames rec =
--   case constrFields (toConstr rec) of
--     [] -> error "Not a record type"
--     fs -> fs

-- -- | Look up the index of a field by name.
-- fieldIndex :: Data r => String -> r -> Int
-- fieldIndex fieldName rec =
--   case elemIndex fieldName (fieldNames rec) of
--     Just i  -> i
--     Nothing -> error $ "No such field: " ++ fieldName

modifyField :: (Data r, Typeable v) => String -> (v -> v) -> r -> r
modifyField fieldName m rec = gmapTi i (e `extT` m) rec
  where
    i = fieldName `fieldIndex` rec
    e x = error $ "Type mismatch: " ++ fieldName ++
                             " :: " ++ (show . typeOf $ x) ++
                           ", not " ++ (show . typeOf $ m undefined)

setField :: (Data r, Typeable v) => String -> v -> r -> r
setField fieldName value = modifyField fieldName (const value)

-- | Get the record field names.
fieldNames :: Data r => r -> [String]
fieldNames rec =
  let fs = constrFields (toConstr rec)
  in if null fs 
       then error "Not a record type or no field names available"
       else fs

-- | Look up the index of a field by name.
fieldIndex :: Data r => String -> r -> Int
fieldIndex fieldName rec =
  case elemIndex fieldName (fieldNames rec) of
    Just i  -> i
    Nothing -> error $ "No such field: " ++ fieldName

----------------------------------------------------------------
-- A safe getField using Dynamic

getField :: forall r v. (Data r, Typeable v) => String -> r -> v
getField fieldName rec =
  let idx :: Int
      idx = fieldIndex fieldName rec
      -- use gmapQ to collect all immediate children (the record fields)
      children :: [Dynamic]
      children = gmapQ toDyn rec
  in if idx < length children then
       case fromDynamic (children !! idx) of
         Just v  -> v
         Nothing -> error $ "Type mismatch for field " ++ fieldName
     else error $ "Index out of bounds for field " ++ fieldName
                           