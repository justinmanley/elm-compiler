{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Optimize.Variable where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Control.Monad.State (State, evalState, modify, get)
import Data.Foldable (foldrM)
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

baseScope :: Scope
baseScope = 
    let saveEnvVar = Var.builtin E.saveEnvName   
    in Map.fromList [(saveEnvVar, Var.Analyzed saveEnvVar 0)] 

uniquify :: Module.CanonicalModule -> Module.AnalyzedModule R.Region
uniquify modul = case modul of
    Module.Module { body, .. } -> case body of
        Module.ModuleBody { program, .. } -> modul { 
            Module.body = body { 
                Module.program = evalState (uniquifyExpr baseScope program) 0 
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

    E.Lambda pat bodyExpr -> do
        (scope', uniquedPat) <- uniquifyPat scope pat 
        
        E.Lambda uniquedPat 
            <$> uniquifyExpr scope' bodyExpr

    E.App appExpr argExpr -> do 
        E.App 
            <$> uniquifyExpr scope appExpr
            <*> uniquifyExpr scope argExpr 
    
    E.MultiIf ifExprs -> 
        -- Want to make sure that the state is passed along.
        let uniquifyIfExpr (ifExpr, thenExpr) = 
                (,) <$> uniquifyExpr scope ifExpr <*> uniquifyExpr scope thenExpr
        in E.MultiIf <$> mapM uniquifyIfExpr ifExprs
        
    E.Let defs bodyExpr -> do
        (scope', uniquedDefs) <- uniquifyDefs scope defs
        
        E.Let uniquedDefs
            <$> uniquifyExpr scope' bodyExpr        

    E.Case targetExpr cases -> 
        let uniquifyCase (casePat, caseExpr) = do
                (scope', uniquedCasePat) <- uniquifyPat scope casePat  
                (,) uniquedCasePat <$> uniquifyExpr scope' caseExpr
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
    -> State Int (Scope, Pat.AnalyzedPattern R.Region)
uniquifyPat scope (A ann pat) = fmap (second $ A ann) $ case pat of 
    Pat.Data cons pats -> do
        (patScopes, uniquifiedPats) <- unzip <$> mapM (uniquifyPat scope) pats
        
        (,) (foldr Map.union scope patScopes) 
            <$> (Pat.Data 
                <$> getVar scope cons 
                <*> return uniquifiedPats)

    Pat.Record fieldNames -> do
        (fieldNameScopes, fieldNameIds) <- unzip <$> mapM (freshVar scope) fieldNames
        
        return (foldr Map.union scope fieldNameScopes, Pat.Record fieldNameIds)

    Pat.Alias alias aliasPat -> do
        (scope', uniquedAlias) <- freshVar scope alias
 
        second (Pat.Alias uniquedAlias)
            <$> uniquifyPat scope' aliasPat

    Pat.Var var -> 
        second Pat.Var <$> freshVar scope var

    Pat.Literal lit -> return (scope, Pat.Literal lit)

    Pat.Anything -> return (scope, Pat.Anything) 

uniquifyRecordField :: Scope 
    -> (String, Canonical.Expr) 
    -> State Int (String, Analyzed.Expr R.Region)
uniquifyRecordField scope (fieldName, expr) =
    (,) fieldName <$> uniquifyExpr scope expr

uniquifyDefs :: Scope
    -> [Canonical.Def]
    -> State Int (Scope, [Analyzed.Def R.Region])
uniquifyDefs scope defs = 
    let uniquifyDef :: Scope
            -> Canonical.Def
            -> State Int (Analyzed.Def R.Region)
        uniquifyDef scope (Canonical.Definition pat expr maybeType) = do 
            (scope', uniquedPat) <- uniquifyPat scope pat    

            Analyzed.Definition uniquedPat
                <$> uniquifyExpr scope' expr
                <*> return maybeType  
   
        -- | Bring the let-bound names into scope.
        collectNames :: Canonical.Def -> Scope -> State Int Scope 
        collectNames (Canonical.Definition pat expr maybeType) scope =
            fst <$> uniquifyPat scope pat 
            
    in do
        defScope <- foldrM collectNames scope defs
        (,) defScope <$> mapM (uniquifyDef defScope) defs

freshVar :: Scope -> String -> State Int (Scope, Var.Analyzed)
freshVar scope name = do
    modify (+1)
    
    let canonicalVar = Var.local name
        var = Var.Analyzed canonicalVar <$> get
    
    (,) <$> (Map.insert canonicalVar <$> var <*> return scope) <*> var
    
getVar :: Scope -> Var.Canonical -> State Int Var.Analyzed
getVar scope canonicalVar = case Map.lookup canonicalVar scope of
    Just var -> return var 
    Nothing -> error . unlines $
        [ "[Compiler error]: Variable " 
        , Var.toString canonicalVar 
        , " should be in scope, but is not." ]

