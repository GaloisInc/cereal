{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Cereal.TH where

import Data.Serialize
import Prelude
import Data.Maybe (isJust)
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import Data.Foldable
import Data.Traversable
import Control.Monad
import Data.Functor
import Data.Text (Text)

makeCerealCustom :: Name -> Name -> Q [Dec]
makeCerealCustom name hv = makeCerealInternal (Just hv) name

makeCerealIdentity :: Name -> Q [Dec]
makeCerealIdentity = makeCerealInternal (Just $ mkName "Identity")

makeCereal :: Name -> Q [Dec]
makeCereal = makeCerealInternal Nothing

makeCerealInternal :: Maybe Name -> Name -> Q [Dec]
makeCerealInternal higherKindType name = do
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
        instanceType = case higherKindType of
                        Just v -> AppT (ConT ''Serialize) $ AppT (ConT datatypeName) (ConT v)
                        _ -> AppT (ConT ''Serialize) $ ConT datatypeName
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
                catchAll :: Q Match
                catchAll = do
                  xName <- newName "x"
                  match
                    (varP xName)
                    (normalB
                      (appE
                        (varE 'fail)
                        (infixApp
                          (infixE
                            (Just (litE (stringL "Unexpected Tag: ")))
                            (varE '(<>))
                            (Just (appE (varE 'show) (varE xName))))
                          (varE '(<>))
                          (infixE
                            (Just (litE (stringL " for type: ")))
                            (varE '(<>))
                            (Just (appE (varE 'show) (litE (stringL $ nameBase name)))))
                        )
                      )
                    ) []
                branchBasedOnConstr =
                  noBindS $
                  caseE (varE constrNameBinding) (matches <> [catchAll])
                bytesToRead = mkName "bytesToRead"
                lenRead = bindS (varP bytesToRead) (varE 'getWord32be)
                body = normalB $ doE [lenRead, noBindS $ appE (appE (varE 'isolate) (appE (varE 'fromEnum) (varE bytesToRead))) (doE [bindCnstrName, branchBasedOnConstr])]
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
            normalB $ appE (appE (varE 'putNested) (uInfixE (varE 'putWord32be) (varE '(.)) (varE 'toEnum))) (doE (putConstr : putStmts))
      pure <$>
        instanceD (pure []) (pure instanceType) (funcDecl)
