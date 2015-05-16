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
type DependencyGraph graph a = graph a Dependency

tailCallGraph :: DynGraph graph 
    => CanonicalModule 
    -> State VarEnv (DependencyGraph graph a)
tailCallGraph modul = case Module.program $ Module.body modul of
    A _ expr -> case expr of
        (E.Let defs expr) -> foldrM (checkDef []) Graph.empty defs
        _ -> undefined -- shouldn't happen

checkDef :: DynGraph graph 
    => [Var.Canonical]
    -> Canonical.Def
    -> DependencyGraph graph a 
    -> State VarEnv (DependencyGraph graph a)
checkDef boundVariables (Canonical.Definition annotatedPat expr _) tailCalls = case annotatedPat of
    A _ pat -> case pat of
        Pattern.Var str        -> do
            (env, var) <- Env.fresh <$> get <*> (return $ Var.Canonical Var.Local str)
            put env
            checkExpr var boundVariables expr tailCalls

        _ -> return tailCalls

checkExpr :: forall graph a . DynGraph graph 
    => Env.UniqueVar
    -> [Var.Canonical]
    -> Canonical.Expr 
    -> DependencyGraph graph a 
    -> State VarEnv (DependencyGraph graph a)
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

    E.ExplicitList exprs                 -> 
        return tailCalls 
        -- This isn't quite accurate - this will ignore functions that are called from these expressions.

    E.Binop binOpVar leftArgExpr rightArgExpr -> do
        leftCalls :: DependencyGraph graph a <- 
            checkExpr var boundVariables leftArgExpr Graph.empty
        
        rightCalls :: DependencyGraph graph a <- 
            checkExpr var boundVariables rightArgExpr Graph.empty

        (env, binOp) <- Env.variable <$> get <*> return binOpVar
        put env

        let calls = (Graph.lsuc leftCalls var) ++ (Graph.lsuc rightCalls var)
            calledInBody (varId, dep) = (var, varId, Body)

        return $ Graph.insEdges ((var, binOp, Tail) : map calledInBody calls) tailCalls

    E.Lambda pat expr'                   -> 
        checkExpr var (bindPat pat boundVariables) expr' tailCalls

    E.App appExpr argExpr                -> undefined --insEdges es tailCalls
        ---- Look at each of the terms that appears tail recursively in argCalls.
        ---- Insert an edge (var, term, Just Tail) only if term does not appear in appCalls.
        -- I want to have a convenient way of checking that a variable does not occur in a term.
        --where
        --appCalls = freeVarsIn appExpr
        --argCalls = checkExpr var argExpr tailCalls

        --argTailCalls :: [(Node, Maybe Dependency)]
        --argTailCalls = lsuc argCalls var

        --tcs :: [Node]
        --tcs = filter (not containedIn appCalls) $ argTailCalls

        --es :: [LEdge (Maybe Dependency)]
        --es = zip3 (replicate (length tcs) $ idOf var) (map idOf tcs) (replicate (length tcs) $ Just Tail) 
    E.MultiIf ifExprs                    -> 
        foldrM (checkIfExpr var boundVariables) tailCalls ifExprs

    E.Let defs expr'                     ->
        checkExpr var boundVariables expr' =<< (foldrM (checkDef boundVariables) tailCalls defs)

    E.Case expr cases                    -> 
        undefined 
    
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

checkIfExpr :: forall graph a . DynGraph graph
    => Env.UniqueVar
    -> [Var.Canonical] 
    -> (Canonical.Expr, Canonical.Expr)
    -> DependencyGraph graph a 
    -> State VarEnv (DependencyGraph graph a)
checkIfExpr var boundVariables (ifExpr, thenExpr) tailCalls = do
        ifCalls :: DependencyGraph graph a <- 
            checkExpr var boundVariables ifExpr Graph.empty

        thenCalls :: DependencyGraph graph a <-
            checkExpr var boundVariables thenExpr Graph.empty

        let ifCalledVars = Graph.lsuc ifCalls var
            thenCalledVars = Graph.lsuc thenCalls var

            addDependency (varId, dep) callGraph = return $ 
                if (varId, dep) `elem` ifCalledVars 
                then Graph.insEdge (var, varId, Body) callGraph 
                else Graph.insEdge (var, varId, dep) callGraph

        foldrM addDependency tailCalls (ifCalledVars ++ thenCalledVars)

-- Is it really necessary to have two bindPat functions? Is this handled by checkDef?
-- When a pattern is encountered at the top level.
-- The only difference is the way that variable patterns are treated!
bindPat :: CanonicalPattern -> [Var.Canonical] -> [Var.Canonical]
bindPat pat@(A _ pat') boundVariables = case pat' of
    Pattern.Var str -> boundVariables
    _ -> bindPat' pat boundVariables

-- If a variable pattern is not at the top level, we treat it like any other pattern,
-- and add the variable to the list of bound variables.
-- Is this necessary?
bindPat' :: CanonicalPattern -> [Var.Canonical] -> [Var.Canonical]
bindPat' (A _ pat) boundVariables = case pat of
    Pattern.Data _ pats -> foldr bindPat boundVariables pats 
    Pattern.Record fieldNames -> (map Var.local fieldNames) ++ boundVariables
    Pattern.Alias str pat' -> bindPat pat' (Var.local str : boundVariables)
    Pattern.Var str -> Var.local str : boundVariables
    _ -> boundVariables
