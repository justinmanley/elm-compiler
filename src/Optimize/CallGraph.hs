{-# LANGUAGE ScopedTypeVariables #-}

module Optimize.CallGraph where

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive (Node, LEdge, gelem)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Foldable (foldrM)

import qualified AST.Expression.Analyzed as Analyzed
import qualified AST.Variable as Var
import qualified AST.Module as Module 
import qualified AST.Pattern as Pattern
import AST.Pattern (CanonicalPattern)
import qualified AST.Expression.General as E
import Reporting.Annotation ( Annotated(A) )
import qualified Optimize.Environment as Env

data Dependency
    = Body
    | Tail
    deriving (Show, Eq)

type VarEnv = Env.Environment Var.Analyzed
type DependencyGraph = Gr () Dependency

callGraph :: Module.AnalyzedModule ann -> DependencyGraph
callGraph modul = 
    let expr@(A _ expr') = Module.program . Module.body $ modul
    in case expr' of
        (E.Let _ _) -> checkTopLevelExpr [] expr Graph.empty 
        _ -> error $ unlines 
            [ "[Compiler error]: The top-level expression in a module" 
            , " should be a let-expression after canonicalization." ] 

checkTopLevelExpr :: [Var.Analyzed]
    -> Analyzed.Expr ann
    -> DependencyGraph
    -> DependencyGraph
checkTopLevelExpr boundVariables (A _ expr) callGr = case expr of
    E.Let defs bodyExpr -> checkTopLevelExpr boundVariables bodyExpr $
        foldr (checkDef boundVariables) callGr defs

    _ -> callGr

checkDef :: [Var.Analyzed]
    -> Analyzed.Def ann
    -> DependencyGraph 
    -> DependencyGraph
checkDef boundVariables (Analyzed.Definition (A _ pat) expr _) tailCalls = case pat of
    Pattern.Var var -> 
        checkExpr var boundVariables expr tailCalls

    _ -> tailCalls

checkExpr :: Var.Analyzed
    -> [Var.Analyzed]
    -> Analyzed.Expr ann 
    -> DependencyGraph 
    -> DependencyGraph
checkExpr var boundVariables (A _ expr) tailCalls = case expr of
    E.Literal _                           -> 
        tailCalls 
    
    E.Var var'                            -> 
        if var' `elem` Analyzed.saveEnvVar:boundVariables
        then tailCalls
        else updateEdge (Var.varId var, Var.varId var', Tail) tailCalls
    
    E.Range lowExpr highExpr              -> 
        let lowCalls = checkExpr var boundVariables lowExpr tailCalls
            highCalls = checkExpr var boundVariables highExpr tailCalls
        in mergeWith body lowCalls highCalls

    E.ExplicitList exprs                  -> 
        let checkExplicitList expr' = checkExpr var boundVariables expr' Graph.empty
        in foldr (mergeWith fixBody) tailCalls $ map checkExplicitList exprs

    E.Binop binOp leftArgExpr rightArgExpr  -> 
        let leftCalls = checkExpr var boundVariables leftArgExpr Graph.empty
            rightCalls = checkExpr var boundVariables rightArgExpr Graph.empty
        in updateEdge (Var.varId var, Var.varId binOp, Tail) $ 
            (mergeWith body) leftCalls rightCalls

    E.Lambda pat expr'                    -> 
        checkExpr var (bindPat pat boundVariables) expr' tailCalls

    E.App appExpr argExpr                 -> 
        let appCalls = checkExpr var boundVariables appExpr tailCalls
            argCalls = checkExpr var boundVariables argExpr tailCalls
        in mergeWith doesNotOccur appCalls argCalls

    E.MultiIf ifExprs                     ->
        let multiIfCalls = map (checkIfExpr var boundVariables Graph.empty) ifExprs
        in foldr (mergeWith fixBody) tailCalls multiIfCalls

    E.Let defs expr'                      ->
        checkExpr var boundVariables expr' $ foldr (checkDef boundVariables) tailCalls defs

    E.Case targetExpr cases               -> 
        let targetExprCalls = checkExpr var boundVariables targetExpr Graph.empty
            caseCalls = map (checkCaseExpr var boundVariables Graph.empty) cases
        in mergeWith doesNotOccur targetExprCalls $ foldr (mergeWith fixBody) tailCalls caseCalls
    
    E.Data _ exprs                        -> 
        let argExprs = map (\expr' -> checkExpr var boundVariables expr' Graph.empty) exprs

        -- Should we do anything with the string representing the data constructor?
        in foldr (mergeWith body) tailCalls argExprs

    E.Access recordExpr _                 -> 
        Graph.emap (const Body) $ checkExpr var boundVariables recordExpr tailCalls

    E.Remove recordExpr _                 -> 
        Graph.emap (const Body) $ checkExpr var boundVariables recordExpr tailCalls

    E.Insert recordExpr _ insertExpr      -> 
        let recordCalls = checkExpr var boundVariables recordExpr tailCalls
            insertCalls = checkExpr var boundVariables insertExpr Graph.empty
        in foldr (mergeWith body) tailCalls [recordCalls, insertCalls] 

    E.Modify recordExpr modifications           -> 
        let recordCalls = checkExpr var boundVariables recordExpr Graph.empty
            modifyCalls = map (\(_, expr') -> checkExpr var boundVariables expr' Graph.empty) modifications
        in foldr (mergeWith body) tailCalls (recordCalls : modifyCalls)

    E.Record record                       -> 
        foldr (mergeWith body) tailCalls $ 
            map (\(_, expr') -> checkExpr var boundVariables expr' Graph.empty) record

    E.Port _                              -> 
        tailCalls

    E.GLShader _ _ _                      -> 
        tailCalls

--checkCaseExpr :: Env.UniqueVar
--    -> [Var.Analyzed]
--    -> DependencyGraph
--    -> (CanonicalPattern, Analyzed.Expr ann)
--    -> State VarEnv (DependencyGraph)
checkCaseExpr var boundVariables tailCalls (pat, expr) = 
    checkExpr var (bindPat pat boundVariables) expr tailCalls
    
--checkIfExpr :: Env.UniqueVar
--    -> [Var.Analyzed] 
--    -> DependencyGraph 
--    -> (Analyzed.Expr ann, Analyzed.Expr ann)
--    -> State VarEnv (DependencyGraph)
checkIfExpr var boundVariables tailCalls (ifExpr, thenExpr) = 
    let ifCalls = checkExpr var boundVariables ifExpr tailCalls
        thenCalls = checkExpr var boundVariables thenExpr tailCalls
    in mergeWith doesNotOccur ifCalls thenCalls

--bindPat :: CanonicalPattern -> [Var.Analyzed] -> [Var.Analyzed]
bindPat (A _ pat) boundVariables = case pat of
    Pattern.Data _ pats -> foldr bindPat boundVariables pats 
    Pattern.Record fieldNames -> fieldNames ++ boundVariables
    Pattern.Alias alias pat' -> bindPat pat' (alias : boundVariables)
    Pattern.Var var -> var : boundVariables
    _ -> boundVariables

--body :: LEdge Dependency -> DependencyGraph -> DependencyGraph
body (var1, var2, _) callGraph = updateEdge (var1, var2, Body) callGraph

-- | Perform a biased merge of the labeled edge (var1, var2, dep) into callGraph
--   so that the Body label will never be replaced by the Tail label.
--fixBody :: LEdge Dependency -> DependencyGraph -> DependencyGraph
fixBody (var1, var2, dep) callGraph = case edge (var1, var2) callGraph of
    Nothing   -> updateEdge (var1, var2, dep) callGraph
    Just (_, _, previousDep) -> case previousDep of
        Tail -> updateEdge (var1, var2, dep) callGraph
        Body -> callGraph

-- | If the target edge appears in the graph, then change its label to Body.
--   Otherwise, leave the dependency label unchanged.
--doesNotOccur :: LEdge Dependency -> DependencyGraph -> DependencyGraph
doesNotOccur (var1, var2, _) callGraph = case edge (var1, var2) callGraph of
    Nothing -> callGraph
    Just _  -> updateEdge (var1, var2, Body) callGraph 

-- | If the graph contains an edge from src to dest, return that edge inside a Just.
--   Otherwise, return Nothing. 
--   Assumes that there can only be one edge in each direction between any pair of edges.
-- i.e. assumes that g is a simple digraph.
--edge :: (Node, Node) -> graph a b -> Maybe (LEdge b)
edge (src, dest) gr = 
    if src `gelem` gr --somehow this is called on a src node which doesn't exist in the graph. Why?
    then case filter (\(_, n2, _) -> n2 == dest) $ Graph.out gr src of
        []        -> Nothing
        labEdge:_ -> Just labEdge
    else Nothing 

mergeWith :: (LEdge Dependency -> DependencyGraph -> DependencyGraph)
    -> DependencyGraph 
    -> DependencyGraph 
    -> DependencyGraph
mergeWith mergeEdge gMajor gMinor = 
    foldr mergeEdge gMajor $ Graph.labEdges gMinor

--updateEdge :: LEdge b -> graph () b -> graph () b
updateEdge (n1, n2, label) g = let
        addNodeIfMissing node graph = 
            if not $ node `gelem` graph 
            then Graph.insNode (node, ()) graph 
            else graph

        g' = (addNodeIfMissing n2 . addNodeIfMissing n1) g
    in case edge (n1, n2) g' of
        Nothing -> Graph.insEdge (n1, n2, label) g'
        Just _  -> Graph.insEdge (n1, n2, label) $ Graph.delEdge (n1, n2) g'

