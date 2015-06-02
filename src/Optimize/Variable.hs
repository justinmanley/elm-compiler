{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Optimize.Variable where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (State, evalState, modify, get)
import qualified Data.Map as Map

import qualified AST.Expression.Analyzed as Analyzed
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as E
import qualified AST.Module as Module 
import qualified AST.Pattern as Pat
import qualified AST.Variable as Var
import Reporting.Annotation ( Annotated(A) ) 
import qualified Reporting.Region as R

type Scope = Map.Map Var.Canonical Var.Analyzed

uniquify :: Module.CanonicalModule -> Module.AnalyzedModule R.Region
uniquify modul = case modul of
    Module.Module { body, .. } -> case body of
        Module.ModuleBody { program, .. } -> modul { 
            Module.body = body { 
                Module.program = evalState (uniquifyExpr Map.empty program) 0 
            } 
        }

uniquifyExpr :: Scope -> Canonical.Expr -> State Int (Analyzed.Expr R.Region)
uniquifyExpr scope (A region expr) = fmap (A region) $ case expr of 
    E.Literal lit -> 
        return $ E.Literal lit
    
    E.Var canonicalVar -> E.Var <$> getVar scope canonicalVar
   
    E.Range lowExpr highExpr -> do 
        low <- uniquifyExpr scope lowExpr 
        high <- uniquifyExpr scope highExpr
    
        return $ E.Range low high        

    E.ExplicitList exprs -> 
        -- Want to make sure that the state is passed along.
        E.ExplicitList <$> mapM (uniquifyExpr scope) exprs

    E.Binop binOpVar leftArgExpr rightArgExpr -> 
        E.Binop 
            <$> getVar scope binOpVar
            <*> uniquifyExpr scope leftArgExpr
            <*> uniquifyExpr scope rightArgExpr

    E.Lambda pat bodyExpr -> 
        E.Lambda <$> uniquifyPat scope pat <*> uniquifyExpr scope bodyExpr

    E.App appExpr argExpr -> do 
        E.App 
            <$> uniquifyExpr scope appExpr
            <*> uniquifyExpr scope argExpr 
    
    E.MultiIf ifExprs -> 
        -- Want to make sure that the state is passed along.
        let uniquifyIfExpr (ifExpr, thenExpr) = 
                (,) <$> uniquifyExpr scope ifExpr <*> uniquifyExpr scope thenExpr
        in E.MultiIf <$> mapM uniquifyIfExpr ifExprs
        
    E.Let defs bodyExpr -> 
        E.Let 
            <$> mapM (uniquifyDef scope) defs 
            <*> uniquifyExpr scope bodyExpr        

    E.Case targetExpr cases -> 
        let uniquifyCase (casePat, caseExpr) = 
                (,) <$> uniquifyPat scope casePat <*> uniquifyExpr scope caseExpr
        in E.Case <$> uniquifyExpr scope targetExpr <*> mapM uniquifyCase cases    

    E.Data cons exprs -> 
        E.Data cons <$> mapM (uniquifyExpr scope) exprs

    E.Access recordExpr field -> 
        E.Access <$> uniquifyExpr scope recordExpr <*> return field

    E.Remove recordExpr field -> 
        E.Remove <$> uniquifyExpr scope recordExpr <*> return field

    E.Insert recordExpr field insertExpr -> 
        E.Insert 
            <$> uniquifyExpr scope recordExpr 
            <*> return field 
            <*> uniquifyExpr scope insertExpr

    E.Modify recordExpr modifications -> 
        E.Modify 
            <$> uniquifyExpr scope recordExpr 
            <*> mapM (uniquifyRecordField scope) modifications

    E.Record record ->  
        E.Record <$> mapM (uniquifyRecordField scope) record

    E.Port portImpl -> fmap E.Port $ case portImpl of
        E.In str tipe -> return $ E.In str tipe
        E.Out str outExpr tipe -> E.Out str 
            <$> uniquifyExpr scope outExpr 
            <*> return tipe
        
        E.Task str taskExpr tipe -> E.Task str 
            <$> uniquifyExpr scope taskExpr 
            <*> return tipe  

    E.GLShader str1 str2 glShaderTipe -> 
        return $ E.GLShader str1 str2 glShaderTipe

uniquifyPat :: Scope
    -> Pat.CanonicalPattern 
    -> State Int (Pat.AnalyzedPattern R.Region)
uniquifyPat scope (A ann pat) = fmap (A ann) $ case pat of 
    Pat.Data cons pats ->
        Pat.Data 
            <$> getVar scope cons
            <*> mapM (uniquifyPat scope) pats

    Pat.Record fieldNames -> return $
         Pat.Record fieldNames 

    Pat.Alias alias aliasPat ->  
        Pat.Alias alias  <$> (uniquifyPat scope aliasPat)

    Pat.Var name -> return $ Pat.Var name 

    Pat.Literal lit -> return $ Pat.Literal lit

    Pat.Anything -> return Pat.Anything 

uniquifyRecordField :: Scope 
    -> (String, Canonical.Expr) 
    -> State Int (String, Analyzed.Expr R.Region)
uniquifyRecordField scope (fieldName, expr) =
    (,) fieldName <$> uniquifyExpr scope expr

uniquifyDef :: Scope
    -> Canonical.Def
    -> State Int (Analyzed.Def R.Region)
uniquifyDef scope (Canonical.Definition pat expr maybeType) = 
    Analyzed.Definition 
        <$> uniquifyPat scope pat
        <*> uniquifyExpr scope expr
        <*> return maybeType  

getVar :: Scope -> Var.Canonical -> State Int Var.Analyzed
getVar scope canonicalVar = case Map.lookup canonicalVar scope of
    Just var -> return var
    Nothing -> modify (+1) >> (Var.Analyzed canonicalVar <$> get)
          
