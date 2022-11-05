{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Cereal.TH where

import Data.Serialize
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import Data.Traversable
import Control.Monad
import Data.Functor
import Data.Text (Text)

makeCereal :: Name -> Q [Dec]
makeCereal name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeName
                 , datatypeVars -- Not supported yet
                 , datatypeCons
                 } -> do
      let
        constrNameStr constructor =
          case (constructorName constructor) of
            Name (OccName occName) _ -> occName
        instanceType = AppT (ConT ''Serialize) $ ConT datatypeName
        funcDecl =
          [ getDecl, putDecl ]
        putDecl = funD 'put (datatypeCons <&> putClause)
        getDecl =
          let
            qSpecificConstructorGetsBindingsAndNames :: Q [(Dec, Name, String)]
            qSpecificConstructorGetsBindingsAndNames =
              for datatypeCons $ \constructor ->
                do
                  let
                    constrName = constrNameStr constructor
                  name <- newName $ "get_" <> constrName
                  decl <- valD (varP name) (getBodyForConstructor constructor) []
                  pure (decl, name, constrName)
            getBody = do
              constrNameBinding <- newName "constructor"
              specificConstructorGetsBindingsAndNames <- qSpecificConstructorGetsBindingsAndNames
              let
                bindCnstrName = bindS (varP constrNameBinding) (appTypeE (varE 'get) (conT ''Text))
                matches :: [Q Match]
                matches =
                  specificConstructorGetsBindingsAndNames <&>
                    (\(_, binding, constrName) ->
                      match (litP (stringL constrName)) (normalB (varE binding)) [])
                branchBasedOnConstr =
                  noBindS $
                  caseE (varE constrNameBinding) matches
                body = normalB $ doE [bindCnstrName, branchBasedOnConstr]
                declrs = specificConstructorGetsBindingsAndNames <&> (\(d, _, _) -> pure d)
              valD (varP 'get) body declrs
          in getBody
        getBodyForConstructor (ConstructorInfo { constructorName, constructorFields }) = do
          attrBindingNames <-
            replicateM (length constructorFields) (newName "attr")
          let
            bindings = attrBindingNames <&> (\v -> bindS (varP v) (varE 'get))
            pureValue = foldl (\app v -> appE app (varE v)) (conE constructorName) attrBindingNames
            returnStmt = noBindS $ appE (varE 'pure) pureValue
            doBlockBody = bindings <> [returnStmt]
          normalB $ doE doBlockBody
        putClause datatypeCon@(ConstructorInfo { constructorName, constructorVariant, constructorFields }) = do
          attrBindingNames <-
            replicateM (length constructorFields) (newName "attr")
          case constructorVariant of
            NormalConstructor ->
              let
                bindings = varP <$> attrBindingNames
              in
                clause [conP constructorName bindings ] (putBody datatypeCon attrBindingNames) []
            RecordConstructor names ->
              let
                bindings =
                  zip names (varP <$> attrBindingNames)
                    <&> (\(name, qPat) -> qPat <&> (name,))
              in clause [recP constructorName bindings ] (putBody datatypeCon attrBindingNames) []
        putBody :: ConstructorInfo -> [Name] -> Q Body
        putBody constructor attrBindingNames =
          let
            putConstr =
              noBindS $ appE (varE 'put) (sigE (litE (stringL (constrNameStr constructor))) (conT ''Text))
            putStmts =
              attrBindingNames <&>
                (\name -> noBindS $ appE (varE 'put) (varE name))
          in
            normalB $ doE (putConstr : putStmts)
      pure <$>
        instanceD (pure []) (pure instanceType) (funcDecl)
