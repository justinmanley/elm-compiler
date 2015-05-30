{-# LANGUAGE RecordWildCards, NamedFieldPuns #-} 
module Optimize.Analyze where

import Data.Set (empty)

import qualified AST.Expression.Analyzed as Analyzed
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as E
import AST.Module (Module(Module), ModuleBody(ModuleBody)
    , body, program 
    , CanonicalModule, AnalyzedModule)
import Optimize.Info (Info(Info)
    , role, contains 
    , RecursionRole(BaseCase, TailRecursiveCase))
import Reporting.Annotation ( Annotated(A) )

analyze :: CanonicalModule -> AnalyzedModule
analyze modul = case modul of
    Module { body, .. } -> case body of
        ModuleBody { program, .. } -> modul { 
            body = body { program = analyzeExpr program } 
        }

analyzeExpr :: Canonical.Expr -> Analyzed.Expr
analyzeExpr (A region expr) = case expr of
    E.Literal lit                             -> 
        A (region, Info { role = BaseCase, contains = empty }) $ E.Literal lit
    E.Var canonicalVar                        -> undefined 
    E.Range lowExpr highExpr                  -> undefined
    E.ExplicitList exprs                      -> undefined
    E.Binop binOpVar leftArgExpr rightArgExpr -> undefined
    E.Lambda pat expr'                        -> undefined
    E.App appExpr argExpr                     -> undefined
    E.MultiIf ifExprs                         -> undefined
    E.Let defs expr'                          -> undefined
    E.Case targetExpr cases                   -> undefined
    E.Data _ exprs                            -> undefined
    E.Access recordExpr _                     -> undefined
    E.Remove recordExpr _                     -> undefined
    E.Insert recordExpr _ insertExpr          -> undefined
    E.Modify recordExpr modifications         -> undefined
    E.Record record                           -> undefined
    E.Port _                                  -> undefined
    E.GLShader _ _ _                          -> undefined


    
