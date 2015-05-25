{-# LANGUAGE ScopedTypeVariables #-}

module Optimize.CallGraph where

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Graph (DynGraph, Node, LEdge, gelem)
import Control.Monad.State
import Control.Applicative ((<$>))
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
import AST.Expression.General (saveEnvName) 

data Dependency
    = Body
    | Tail
    deriving (Show, Eq)

type VarEnv = Env.Environment Var.Canonical
type DependencyGraph graph = graph () Dependency

callGraph :: DynGraph graph 
    => CanonicalModule 
    -> State VarEnv (DependencyGraph graph)
callGraph modul = 
    let expr@(A _ expr') = Module.program . Module.body $ modul
    in case expr' of
        (E.Let _ _) -> checkTopLevelExpr [] expr Graph.empty 
        _              -> error $ unlines 
            [ "[Compiler error]: The top-level expression in a module" 
            , " should be a let-expression after canonicalization." ] 

checkTopLevelExpr :: DynGraph graph
    => [Var.Canonical]
    -> Canonical.Expr
    -> DependencyGraph graph
    -> State VarEnv (DependencyGraph graph)
checkTopLevelExpr boundVariables (A _ expr) callGr = case expr of
    (E.Let defs bodyExpr) -> 
        (checkTopLevelExpr boundVariables bodyExpr) =<< (foldrM (checkDef boundVariables) callGr defs)
    _ -> return callGr

checkDef :: DynGraph graph 
    => [Var.Canonical]
    -> Canonical.Def
    -> DependencyGraph graph 
    -> State VarEnv (DependencyGraph graph)
checkDef boundVariables (Canonical.Definition annotatedPat expr _) tailCalls = case annotatedPat of
    A _ pat -> case pat of
        Pattern.Var str -> do
            var <- Env.freshVar (Var.Canonical { Var.home = Var.Local, Var.name =  str })
            checkExpr var boundVariables expr tailCalls

        _ -> return tailCalls

checkExpr :: forall graph . DynGraph graph 
    => Env.UniqueVar
    -> [Var.Canonical]
    -> Canonical.Expr 
    -> DependencyGraph graph 
    -> State VarEnv (DependencyGraph graph)
checkExpr var boundVariables (A _ expr) tailCalls = case expr of
    E.Literal _                           -> 
        return tailCalls 
    
    E.Var canonicalVar                    -> 
        if canonicalVar `elem` saveEnvVar:boundVariables
        then return tailCalls
        else do
            var' <- Env.getVar canonicalVar 
            return $ updateEdge (var, var', Tail) tailCalls
    
    E.Range lowExpr highExpr              -> do
        lowCalls :: DependencyGraph graph <-
            checkExpr var boundVariables lowExpr tailCalls

        highCalls :: DependencyGraph graph <-
            checkExpr var boundVariables highExpr tailCalls

        return $ mergeWith body lowCalls highCalls

    E.ExplicitList exprs                  -> do
        explicitListCalls :: [DependencyGraph graph] <-
            mapM (\expr'  -> checkExpr var boundVariables expr' Graph.empty) exprs

        return $ foldr (mergeWith bodyReplacesTail) tailCalls explicitListCalls

    E.Binop binOpVar leftArgExpr rightArgExpr  -> do
        leftCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables leftArgExpr Graph.empty
        
        rightCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables rightArgExpr Graph.empty

        binOp <- Env.getVar binOpVar
        return $ (updateEdge (var, binOp, Tail) $ (mergeWith body) leftCalls rightCalls)

    E.Lambda pat expr'                    -> 
        checkExpr var (bindPat pat boundVariables) expr' tailCalls

    E.App appExpr argExpr                 -> do
        appCalls <- checkExpr var boundVariables appExpr tailCalls
        argCalls <- checkExpr var boundVariables argExpr tailCalls

        return $ (mergeWith doesNotOccur) appCalls argCalls

    E.MultiIf ifExprs                     -> do
        multiIfCalls :: [DependencyGraph graph] <- 
            mapM (checkIfExpr var boundVariables Graph.empty) ifExprs

        return $ foldr (mergeWith bodyReplacesTail) tailCalls multiIfCalls

    E.Let defs expr'                      ->
        checkExpr var boundVariables expr' =<< (foldrM (checkDef boundVariables) tailCalls defs)

    E.Case targetExpr cases               -> do
        targetExprCalls :: DependencyGraph graph <- 
            checkExpr var boundVariables targetExpr Graph.empty

        caseCalls :: [DependencyGraph graph] <- 
            mapM (checkCaseExpr var boundVariables Graph.empty) cases

        return $ (mergeWith doesNotOccur) targetExprCalls (foldr (mergeWith bodyReplacesTail) tailCalls caseCalls)
    
    E.Data _ exprs                        -> do
        argExprs :: [DependencyGraph graph] <-
            mapM (\expr' -> checkExpr var boundVariables expr' Graph.empty) exprs

        -- Should we do anything with the string representing the data constructor?
        return $ foldr (mergeWith body) tailCalls argExprs

    E.Access recordExpr _                 -> 
        Graph.emap (const Body) <$> (checkExpr var boundVariables recordExpr tailCalls)

    E.Remove recordExpr _                 -> 
        Graph.emap (const Body) <$> (checkExpr var boundVariables recordExpr tailCalls)

    E.Insert recordExpr _ insertExpr      -> do
        recordCalls :: DependencyGraph graph <-
            checkExpr var boundVariables recordExpr tailCalls

        insertCalls :: DependencyGraph graph <-
            checkExpr var boundVariables insertExpr Graph.empty

        return $ foldr (mergeWith body) tailCalls [recordCalls, insertCalls] 

    E.Modify recordExpr modifications           -> do
        recordCalls :: DependencyGraph graph <-
            checkExpr var boundVariables recordExpr Graph.empty

        modifyCalls :: [DependencyGraph graph] <-
            mapM (\(_, expr') -> checkExpr var boundVariables expr' Graph.empty) modifications

        return $ foldr (mergeWith body) tailCalls (recordCalls : modifyCalls)

    E.Record record                       -> 
        foldr (mergeWith body) tailCalls <$> 
            mapM (\(_, expr') -> checkExpr var boundVariables expr' Graph.empty) record

    E.Port _                              -> 
        return tailCalls

    E.GLShader _ _ _                      -> 
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

body :: DynGraph graph => LEdge Dependency -> DependencyGraph graph -> DependencyGraph graph
body (var1, var2, _) callGraph = updateEdge (var1, var2, Body) callGraph

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
doesNotOccur (var1, var2, _) callGraph = case edge (var1, var2) callGraph of
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

saveEnvVar :: Var.Canonical
saveEnvVar = Var.Canonical { Var.home = Var.BuiltIn, Var.name = saveEnvName }
