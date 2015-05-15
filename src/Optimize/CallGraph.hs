{-# LANGUAGE NamedFieldPuns #-}

module Optimize.CallGraph where

import Data.Graph.Inductive.Graph (
    DynGraph, Node, LEdge, 
    insEdge, insEdges, 
    lsuc)
import qualified Data.Graph.Inductive.Graph as Graph
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (foldrM)

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Variable as Var
import qualified AST.Module as Module 
import qualified AST.Pattern as Pattern
import qualified AST.Expression.General as E
import Reporting.Annotation ( Annotated(A) )
import AST.Module ( CanonicalModule )

data Dependency
    = Body
    | Tail

type UniqueVar = Int 

data Environment a = Environment
    { varMap :: Map UniqueVar a 
    , maxIndex :: UniqueVar }

fresh :: Environment a -> a -> (Environment a, UniqueVar)
fresh (Environment { varMap, maxIndex }) var = 
    (Environment 
        { varMap = Map.insert maxIndex var $ varMap
        , maxIndex = maxIndex + 1 }
    , maxIndex)

empty :: Environment a
empty = Environment { varMap = Map.empty, maxIndex = 0 }

type VarEnv = Environment Var.Canonical
type DependencyGraph graph a = graph a (Maybe Dependency)

tailCallGraph :: DynGraph graph 
    => CanonicalModule 
    -> State VarEnv (DependencyGraph graph a)
tailCallGraph modul = case Module.program $ Module.body modul of
    A _ expr -> case expr of
        (E.Let defs expr) -> foldrM checkDef Graph.empty defs
        _ -> undefined -- shouldn't happen

checkDef :: DynGraph graph 
    => Canonical.Def 
    -> DependencyGraph graph a 
    -> State VarEnv (DependencyGraph graph a)
checkDef (Canonical.Definition annotatedPat expr _) tailCalls = case annotatedPat of
    A _ pat -> case pat of
        Pattern.Var str        -> do
            (env, var) <- fresh <$> get <*> (return $ Var.Canonical Var.Local str)
            put env
            checkExpr var expr tailCalls

        _ -> return tailCalls

checkExpr :: DynGraph graph 
    => UniqueVar
    -> Canonical.Expr 
    -> DependencyGraph graph a 
    -> State VarEnv (DependencyGraph graph a)
checkExpr var (A _ expr) tailCalls = case expr of
    E.Literal lit                        -> 
        return tailCalls 
    
    E.Var canonicalVar                   -> do
        (env, var') <- fresh <$> get <*> return canonicalVar
        put env
        return $ insEdge (var, var', Just Tail) tailCalls
    
    E.Range lowExpr highExpr             -> 
        return tailCalls 

    E.ExplicitList exprs                 -> 
        return tailCalls 

    E.Binop var leftArgExpr rightArgExpr -> undefined 
    E.Lambda pat expr'                   -> undefined
    E.App appExpr argExpr                -> undefined --insEdges es tailCalls
        ---- Look at each of the terms that appears tail recursively in argCalls.
        ---- Insert an edge (var, term, Just Tail) only if term does not appear in appCalls.
        --where
        --appCalls = freeVarsIn appExpr
        --argCalls = checkExpr var argExpr tailCalls

        --argTailCalls :: [(Node, Maybe Dependency)]
        --argTailCalls = lsuc argCalls var

        --tcs :: [Node]
        --tcs = filter (not containedIn appCalls) $ argTailCalls

        --es :: [LEdge (Maybe Dependency)]
        --es = zip3 (replicate (length tcs) $ idOf var) (map idOf tcs) (replicate (length tcs) $ Just Tail) 
    E.MultiIf ifExprs                    -> undefined 
    E.Let defs expr'                     -> undefined
    E.Case expr cases                    -> undefined 
    E.Data str exprs                     -> undefined
    E.Access _ _                         -> 
        return tailCalls

    E.Remove _ _                         -> 
        return tailCalls

    E.Insert _ _ _                       -> 
        return tailCalls

    E.Modify _ _                         -> 
        return tailCalls

    E.Record _                           -> 
        return tailCalls

    E.Port _                             -> 
        return tailCalls

    E.GLShader _ _ _                     -> 
        return tailCalls


----freeVarsIn :: [Var] -> Canonical.Expr -> [Var]
----freeVarsIn boundVariables (A _ expr) = case expr of
----    E.Literal lit                        -> []
----    E.Var var'                           -> [var']
----    E.Range lowExpr highExpr             -> freeVarsIn lowExpr ++ freeVarsIn highExpr 
----    E.ExplicitList exprs                 -> concatMap freeVarsIn exprs
----    E.Binop var leftArgExpr rightArgExpr -> [var] ++ freeVarsIn leftArgExpr ++ freeVarsIn rightArgExpr
----    E.Lambda pat expr'                   -> freeVarsIn (bindPat pat boundVariables) expr'
----    E.App appExpr argExpr                -> freeVarsIn appExpr ++ freeVarsIn argExpr
----    E.MultiIf ifExprs                    -> concatMap freeVarsIn ifExprs
----    E.Let defs expr'                     -> freeVarsIn (foldr bindDef boundVariables defs) expr'
----    E.Case expr cases                    -> bindCases
----    E.Data str exprs                     -> undefined
----    E.Access _ _                         -> tailCalls
----    E.Remove _ _                         -> tailCalls
----    E.Insert _ _ _                       -> tailCalls
----    E.Modify _ _                         -> tailCalls
----    E.Record _                           -> tailCalls
----    E.Port _                             -> []
----    E.GLShader _ _ _                     -> []

----bindDef def boundVariables = undefined

----bindPat pat boundVariables = undefined
