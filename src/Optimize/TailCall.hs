module Optimize.TailCall where

import Data.Graph.Inductive (DynGraph)

import qualified AST.Module as Module
import Optimize.CallGraph (DependencyGraph, callGraph) 
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import Reporting.Annotation ( Annotated(A) )

stronglyConnectedComponents :: Graph graph
    => graph a b
    -> [graph a b]

data SCC graph a b
    = AcyclicSCC a
    | CyclicSCC (graph a b)

eliminateTailCalls :: Module.CanonicalModule -> Module.CanonicalModule
eliminateTailCalls modul = modul { body = 
        body { program = optimizedProgram } 
    } where 
        body = Module.body modul
        optimizedProgram = tceExpr . Module.program $ body

tceExpr :: DynGraph graph 
    => DependencyGraph graph
    -> Canonical.Expr 
tceExpr g (A _ expr) = case expr of
    E.Literal _                           -> 
    E.Var canonicalVar                    -> 
    E.Range lowExpr highExpr              -> do
    E.ExplicitList exprs                  -> do
    E.Binop binOpVar leftArgExpr rightArgExpr  -> do
    E.Lambda pat expr'                    -> 
    E.App appExpr argExpr                 -> do
    E.MultiIf ifExprs                     -> do
    E.Let defs expr'                      ->
    E.Case targetExpr cases               -> do
    E.Data _ exprs                        -> do
    E.Access recordExpr _                 -> 
    E.Remove recordExpr _                 -> 
    E.Insert recordExpr _ insertExpr      -> do
    E.Modify recordExpr modifications           -> do
    E.Record record                       -> 
    E.Port _                              -> 
    E.GLShader _ _ _                      -> 


  
