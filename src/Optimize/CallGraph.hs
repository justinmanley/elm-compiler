{-# LANGUAGE ScopedTypeVariables #-}

module Optimize.CallGraph where

import Data.Graph.Inductive.Graph (DynGraph, Node, LEdge, gelem)
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
            return $ updateEdge (var, var', Tail) tailCalls
    
    E.Range lowExpr highExpr             -> do
        lowCalls :: DependencyGraph graph <-
            checkExpr var boundVariables lowExpr tailCalls

        highCalls :: DependencyGraph graph <-
            checkExpr var boundVariables highExpr tailCalls

        return $ Graph.emap (const Body) (mergeWith bodyReplacesTail lowCalls highCalls)

    E.ExplicitList exprs                 -> do
        explicitListCalls :: [DependencyGraph graph] <-
            mapM (\expr' -> checkExpr var boundVariables expr' Graph.empty) exprs

        return $ foldr (mergeWith bodyReplacesTail) tailCalls explicitListCalls

    E.Binop binOpVar leftArgExpr rightArgExpr -> do
        leftCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables leftArgExpr Graph.empty
        
        rightCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables rightArgExpr Graph.empty

        (env, binOp) <- Env.variable <$> get <*> return binOpVar
        put env

        let calls = (Graph.lsuc leftCalls var) ++ (Graph.lsuc rightCalls var)
            calledInBody (varId, dep) = (var, varId, Body)

        let binOpCalls = updateEdge (var, binOp, Tail) $ (mergeWith bodyReplacesTail) leftCalls rightCalls

        return $ Graph.emap (const Body) binOpCalls

    E.Lambda pat expr'                   -> 
        checkExpr var (bindPat pat boundVariables) expr' tailCalls

    E.App appExpr argExpr                -> do
        appCalls <- checkExpr var boundVariables appExpr tailCalls
        argCalls <- checkExpr var boundVariables argExpr tailCalls

        return $ (mergeWith doesNotOccur) appCalls argCalls

    E.MultiIf ifExprs                    -> do
        multiIfCalls :: [DependencyGraph graph] <- 
            mapM (checkIfExpr var boundVariables Graph.empty) ifExprs

        return $ foldr (mergeWith bodyReplacesTail) tailCalls multiIfCalls

    E.Let defs expr'                     ->
        checkExpr var boundVariables expr' =<< (foldrM (checkDef boundVariables) tailCalls defs)

    E.Case targetExpr cases              -> do
        targetExprCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables targetExpr Graph.empty

        caseCalls :: [DependencyGraph graph] <- 
            mapM (checkCaseExpr var boundVariables Graph.empty) cases

        return $ (mergeWith doesNotOccur) targetExprCalls (foldr (mergeWith bodyReplacesTail) tailCalls caseCalls)
    
    E.Data str exprs                     -> 
        undefined
        -- Is it possible for this to be tail-recursive?

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
    -> DependencyGraph graph
    -> (CanonicalPattern, Canonical.Expr)
    -> State VarEnv (DependencyGraph graph)
checkCaseExpr var boundVariables tailCalls (pat, expr) = 
    checkExpr var (bindPat pat boundVariables) expr tailCalls
    
checkIfExpr :: forall graph . DynGraph graph
    => Env.UniqueVar
    -> [Var.Canonical] 
    -> DependencyGraph graph 
    -> (Canonical.Expr, Canonical.Expr)
    -> State VarEnv (DependencyGraph graph)
checkIfExpr var boundVariables tailCalls (ifExpr, thenExpr) = do
    ifCalls :: DependencyGraph graph <- 
        checkExpr var boundVariables ifExpr tailCalls

    thenCalls :: DependencyGraph graph <-
        checkExpr var boundVariables thenExpr tailCalls

    return $ (mergeWith doesNotOccur) ifCalls thenCalls

bindPat :: CanonicalPattern -> [Var.Canonical] -> [Var.Canonical]
bindPat (A _ pat) boundVariables = case pat of
    Pattern.Data _ pats -> foldr bindPat boundVariables pats 
    Pattern.Record fieldNames -> (map Var.local fieldNames) ++ boundVariables
    Pattern.Alias str pat' -> bindPat pat' (Var.local str : boundVariables)
    Pattern.Var str -> Var.local str : boundVariables
    _ -> boundVariables

-- | Perform a biased merge of the labeled edge (var1, var2, dep) into callGraph
--   so that the Body label will never be replaced by the Tail label.
bodyReplacesTail :: DynGraph graph => LEdge Dependency -> DependencyGraph graph -> DependencyGraph graph
bodyReplacesTail (var1, var2, dep) callGraph = case edge (var1, var2) callGraph of
    Nothing   -> updateEdge (var1, var2, dep) callGraph
    Just (_, _, previousDep) -> case previousDep of
        Tail -> updateEdge (var1, var2, dep) callGraph
        Body -> callGraph

-- | If the target edge appears in the graph, then change its label to Body.
--   Otherwise, leave the dependency label unchanged.
doesNotOccur :: DynGraph graph => LEdge Dependency -> DependencyGraph graph -> DependencyGraph graph
doesNotOccur (var1, var2, dep) callGraph = case edge (var1, var2) callGraph of
    Nothing -> callGraph
    Just _  -> updateEdge (var1, var2, Body) callGraph 

-- | If the graph contains an edge from src to dest, return that edge inside a Just.
--   Otherwise, return Nothing. 
--   Assumes that there can only be one edge in each direction between any pair of edges.
edge :: DynGraph graph => (Node, Node) -> graph a b -> Maybe (LEdge b)
edge (src, dest) gr = case filter (\(_, n2, _) -> n2 == dest) $ Graph.out gr src of
    []        -> Nothing
    labEdge:_ -> Just labEdge

mergeWith :: DynGraph graph
    => (LEdge Dependency -> DependencyGraph graph -> DependencyGraph graph)
    -> DependencyGraph graph 
    -> DependencyGraph graph 
    -> DependencyGraph graph
mergeWith mergeEdge gMajor gMinor = 
    foldr mergeEdge gMajor $ Graph.labEdges gMinor

updateEdge :: DynGraph graph => LEdge b -> graph () b -> graph () b
updateEdge (n1, n2, label) g = let
        addNodeIfMissing node graph = 
            if not $ node `gelem` graph 
            then Graph.insNode (node, ()) graph 
            else graph

        g' = (addNodeIfMissing n2 . addNodeIfMissing n1) g
    in case edge (n1, n2) g' of
        Nothing -> Graph.insEdge (n1, n2, label) g'
        Just _  -> Graph.insEdge (n1, n2, label) $ Graph.delEdge (1,2) g'
