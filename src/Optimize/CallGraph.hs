{-# LANGUAGE ScopedTypeVariables #-}

module Optimize.CallGraph where

import Data.Graph.Inductive.Graph (DynGraph, Node, LEdge)
import qualified Data.Graph.Inductive.Graph as Graph
import Control.Monad.State
import qualified Data.Map.Strict as Map 
import Control.Applicative ((<$>), (<*>))
import Data.Foldable (foldrM)

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Variable as Var
import qualified AST.Module as Module 
import qualified AST.Pattern as Pattern
import AST.Pattern (CanonicalPattern)
import qualified AST.Expression.General as E
import Reporting.Annotation ( Annotated(A) )
import AST.Module ( CanonicalModule )

import qualified Optimize.Environment as Env

data Dependency
    = Body
    | Tail
    deriving (Show, Eq)

type VarEnv = Env.Environment Var.Canonical
type DependencyGraph graph = graph () Dependency

tailCallGraph :: DynGraph graph 
    => CanonicalModule 
    -> State VarEnv (DependencyGraph graph)
tailCallGraph modul = case Module.program $ Module.body modul of
    A _ expr -> case expr of
        (E.Let defs expr) -> foldrM (checkDef []) Graph.empty defs
        _ -> undefined -- shouldn't happen

checkDef :: DynGraph graph 
    => [Var.Canonical]
    -> Canonical.Def
    -> DependencyGraph graph 
    -> State VarEnv (DependencyGraph graph)
checkDef boundVariables (Canonical.Definition annotatedPat expr _) tailCalls = case annotatedPat of
    A _ pat -> case pat of
        Pattern.Var str        -> do
            (env, var) <- Env.fresh <$> get <*> (return $ Var.Canonical Var.Local str)
            put env
            checkExpr var boundVariables expr tailCalls

        _ -> return tailCalls

checkExpr :: forall graph . DynGraph graph 
    => Env.UniqueVar
    -> [Var.Canonical]
    -> Canonical.Expr 
    -> DependencyGraph graph 
    -> State VarEnv (DependencyGraph graph)
checkExpr var boundVariables (A _ expr) tailCalls = case expr of
    E.Literal lit                        -> 
        return tailCalls 
    
    E.Var canonicalVar                   -> 
        if canonicalVar `elem` boundVariables
        then return tailCalls
        else do
            (env, var') <- Env.variable <$> get <*> return canonicalVar
            put env
            return $ Graph.insEdge (var, var', Tail) tailCalls
    
    E.Range lowExpr highExpr             -> 
        return tailCalls 
        -- This isn't quite accurate - this will ignore functions that are called from these expressions.
        -- Certainly there can't be any tail calls in this kind of expression. But I do want to record which
        -- terms appear in these expressions.

    E.ExplicitList exprs                 -> 
        return tailCalls 
        -- This isn't quite accurate - this will ignore functions that are called from these expressions.

    E.Binop binOpVar leftArgExpr rightArgExpr -> do
        leftCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables leftArgExpr Graph.empty
        
        rightCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables rightArgExpr Graph.empty

        (env, binOp) <- Env.variable <$> get <*> return binOpVar
        put env

        let calls = (Graph.lsuc leftCalls var) ++ (Graph.lsuc rightCalls var)
            calledInBody (varId, dep) = (var, varId, Body)

        return $ Graph.insEdges ((var, binOp, Tail) : map calledInBody calls) tailCalls

    E.Lambda pat expr'                   -> 
        checkExpr var (bindPat pat boundVariables) expr' tailCalls

    E.App appExpr argExpr                -> do
        appCalls :: DependencyGraph graph <-
            checkExpr var boundVariables appExpr Graph.empty

        argCalls :: DependencyGraph graph <-
            checkExpr var boundVariables argExpr Graph.empty

        let calledInApp = Graph.lsuc appCalls var
            calledInArg = Graph.lsuc argCalls var

            addDependency (var', dep) callGraph = return $
                if (var', dep) `elem` calledInApp 
                then Graph.insEdge (var, var', Body) callGraph
                else Graph.insEdge (var, var', dep) callGraph

        foldrM addDependency tailCalls (calledInApp ++ calledInArg)

    E.MultiIf ifExprs                    -> 
        foldrM (checkIfExpr var boundVariables) tailCalls ifExprs

    E.Let defs expr'                     ->
        checkExpr var boundVariables expr' =<< (foldrM (checkDef boundVariables) tailCalls defs)

    E.Case targetExpr cases              -> do
        targetExprCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables targetExpr tailCalls

        foldrM (checkCaseExpr var boundVariables) Graph.empty cases
    
    E.Data str exprs                     -> 
        undefined
    
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

checkCaseExpr :: forall graph . DynGraph graph
    => Env.UniqueVar
    -> [Var.Canonical]
    -> (CanonicalPattern, Canonical.Expr)
    -> DependencyGraph graph
    -> State VarEnv (DependencyGraph graph)
checkCaseExpr var boundVariables (pat, expr) tailCalls = 
    checkExpr var (bindPat pat boundVariables) expr tailCalls

updateEdge :: DynGraph graph 
    => (Env.UniqueVar, Env.UniqueVar, Dependency)
    -> DependencyGraph graph
    -> DependencyGraph graph
updateEdge (var1, var2, dep) callGraph = let
        edges = filter (\(v2', _) -> v2' == var2) $ Graph.lsuc callGraph var1
        updateDependencyStatus (v1, v2, dep') gr = case dep' of
            Tail -> Graph.insEdge (v1, v2, dep) gr
            Body -> gr
    in case zipWith (\a (b,c) -> (a,b,c)) (replicate (length edges) var1) edges of 
        []    -> Graph.insEdge (var1, var2, dep) callGraph
        es -> foldr updateDependencyStatus callGraph es

checkIfExpr :: forall graph . DynGraph graph
    => Env.UniqueVar
    -> [Var.Canonical] 
    -> (Canonical.Expr, Canonical.Expr)
    -> DependencyGraph graph 
    -> State VarEnv (DependencyGraph graph)
checkIfExpr var boundVariables (ifExpr, thenExpr) tailCalls = do
        ifCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables ifExpr Graph.empty

        thenCalls :: DependencyGraph graph <-
            checkExpr var boundVariables thenExpr Graph.empty

        let ifCalledVars = Graph.lsuc ifCalls var
            thenCalledVars = Graph.lsuc thenCalls var

            addDependency (varId, dep) callGraph = return $ 
                if (varId, dep) `elem` ifCalledVars 
                then Graph.insEdge (var, varId, Body) callGraph 
                else Graph.insEdge (var, varId, dep) callGraph

        foldrM addDependency tailCalls (ifCalledVars ++ thenCalledVars)

bindPat :: CanonicalPattern -> [Var.Canonical] -> [Var.Canonical]
bindPat (A _ pat) boundVariables = case pat of
    Pattern.Data _ pats -> foldr bindPat boundVariables pats 
    Pattern.Record fieldNames -> (map Var.local fieldNames) ++ boundVariables
    Pattern.Alias str pat' -> bindPat pat' (Var.local str : boundVariables)
    Pattern.Var str -> Var.local str : boundVariables
    _ -> boundVariables
