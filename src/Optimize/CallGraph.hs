module Optimize.CallGraph where

import Data.Graph.Inductive.Graph (DynGraph, empty)

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Variable as Var
import Reporting.Annotation ( Annotated(A) )
import qualified AST.Module as Module 
import AST.Module ( CanonicalModule )
import qualified AST.Pattern as Pattern
import qualified AST.Expression.General as E

data Dependency = TailRecursiveDependency Var.Canonical Var.Canonical

tailCallGraph :: DynGraph graph => CanonicalModule -> graph a b
tailCallGraph modul = case Module.program $ Module.body modul of
    A _ expr -> case expr of
        (E.Let defs expr) -> foldr checkDef empty defs
        _ -> undefined -- shouldn't happen

checkDef :: DynGraph graph 
    => Canonical.Def 
    -> graph a b 
    -> graph a b
checkDef (Canonical.Definition annotatedPat expr _) tailCalls = case annotatedPat of
    A _ pat -> case pat of
        Pattern.Var str        -> checkExpr str expr tailCalls
        Pattern.Data var pats  -> tailCalls
        Pattern.Alias str pat' -> tailCalls 
        _              -> tailCalls 

checkExpr :: DynGraph graph 
    => String 
    -> Canonical.Expr 
    -> graph a b 
    -> graph a b
checkExpr var expr tailCalls = undefined
