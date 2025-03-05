{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module DeriveStorable (deriveStorable) where

import Foreign.Storable
import Foreign.Ptr (Ptr, castPtr)
import Language.Haskell.TH

deriveStorable :: Name -> Q [Dec]
deriveStorable typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> do
      let floatSizeExp = [| sizeOf (undefined :: Float) |]
      -- totalSizeExp is a Q Exp; we then extract its Exp using <-
      totalSizeExp <- foldr (\_ acc -> [| $floatSizeExp + $acc |]) [| 0 |] fields

      -- For peek: generate a fresh variable for each field.
      varNames <- mapM (\_ -> newName "x") fields

      let peekBindings =
            [ BindS (VarP var)
                    (AppE (AppE (VarE 'peekElemOff)
                                (VarE (mkName "p")))
                          (LitE (IntegerL i)))
            | (i, var) <- zip [0..] varNames ]
          peekReturn =
            NoBindS (AppE (VarE 'return)
                           (foldl AppE (ConE conName)
                              (map VarE varNames)))
          letP =
            LetS [ ValD (VarP (mkName "p"))
                        (NormalB (SigE (AppE (VarE 'castPtr)
                                              (VarE (mkName "ptr")))
                                          (AppT (ConT ''Ptr) (ConT ''Float))))
                        [] ]
          peekDo = DoE Nothing (letP : (peekBindings ++ [peekReturn]))

      -- For poke: generate a pattern for the constructor by creating fresh names for each field.
      fieldPats <- mapM (\(fname,_,_) -> newName (nameBase fname)) fields
      let pokePattern = ConP conName [] (map VarP fieldPats)
          pokeBindings =
            [ NoBindS (AppE (AppE (AppE (VarE 'pokeElemOff)
                                          (VarE (mkName "p")))
                                   (LitE (IntegerL i)))
                             (VarE v))
            | (i, v) <- zip [0..] fieldPats ]
          pokeLet =
            LetS [ ValD (VarP (mkName "p"))
                        (NormalB (SigE (AppE (VarE 'castPtr)
                                              (VarE (mkName "ptr")))
                                          (AppT (ConT ''Ptr) (ConT ''Float))))
                        [] ]
          pokeDo = DoE Nothing (pokeLet : pokeBindings)

      inst <- instanceD
                (cxt [])
                (appT (conT ''Storable) (conT typeName))
                [ funD 'sizeOf [clause [wildP] (normalB (return totalSizeExp)) []]
                , funD 'alignment [clause [wildP] (normalB [| alignment (undefined :: Float) |]) []]
                , funD 'peek [clause [varP (mkName "ptr")] (normalB (return peekDo)) []]
                -- Note: the clause expects Q Pat values, so we wrap pokePattern in return.
                , funD 'poke [clause [varP (mkName "ptr"), return pokePattern] (normalB (return pokeDo)) []]
                ]
      return [inst]
    _ -> fail "deriveStorable: Expected a record with a single constructor"
