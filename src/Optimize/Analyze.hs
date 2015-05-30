{-# LANGUAGE RecordWildCards, NamedFieldPuns #-} 
module Optimize.Analyze where

import Data.Set (empty)

import qualified AST.Expression.Analyzed as Analyzed
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as E
import AST.Module (Module(Module), ModuleBody(ModuleBody)
    , body, program 
    , CanonicalModule, AnalyzedModule)
import qualified AST.Variable as Var
import qualified Optimize.Info as Optimize 
import Optimize.Info (Info(Info), role, contains)
import Reporting.Annotation ( Annotated(A) )
import qualified Reporting.Region as R

analyze :: AnalyzedModule R.Region -> AnalyzedModule (R.Region, Optimize.Info)
analyze modul = case modul of
    Module { body, .. } -> case body of
        ModuleBody { program, .. } -> modul { 
            body = body { program = analyzeTopLevelExpr program } 
        }

analyzeTopLevelExpr :: Analyzed.Expr R.Region -> Analyzed.Expr (R.Region, Optimize.Info)
analyzeTopLevelExpr (A region expr) = case expr of
    E.Let defs bodyExpr -> A (region, Info { role = Optimize.BaseCase, contains = empty }) $ 
        E.Let (map analyzeDef defs) $ analyzeTopLevelExpr bodyExpr 
    _     -> error . unlines $ 
        [ "Compiler error: The top-level expression in a "
        , "canonical module should be a let-expression." ]

analyzeDef :: Analyzed.Def R.Region -> Analyzed.Def (R.Region, Optimize.Info)
analyzeDef def = undefined

analyzeExpr :: Var.Analyzed 
    -> Analyzed.Expr R.Region 
    -> Analyzed.Expr (R.Region, Optimize.Info)
analyzeExpr var (A region expr) = case expr of
    E.Literal lit                             -> 
        A (region, Info { role = Optimize.BaseCase, contains = empty }) $ E.Literal lit
    E.Var var'                                -> 
        let role =
                if var' `inSameSCC` var 
                then Optimize.TailRecursiveCase 
                else Optimize.BaseCase 
        in A (region, Info { role = role, contains = empty }) $ E.Var var'  
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

-- | Returns true if two variables are mutually reachable in the call graph.
inSameSCC = undefined    
