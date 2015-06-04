module Optimize.TailCall where

import Control.Applicative ((<$>))
import Data.Graph.Inductive (DynGraph)
import qualified Data.Graph.Inductive.Query.DFS as Graph
import Data.List (find)
import Data.Maybe (isJust)

import qualified AST.Expression.Analyzed as Analyzed
import qualified AST.Module as Module
import qualified AST.Pattern as Pat
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import Optimize.CallGraph (DependencyGraph, callGraph) 
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import Reporting.Annotation ( Annotated(A) )
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R

type Info = (R.Region, [Var.Analyzed])

type StronglyConnComp = [Var.VarId]

eliminateTailCalls :: Module.AnalyzedModule Info -> Module.AnalyzedModule Info
eliminateTailCalls modul = modul { Module.body = 
        body { Module.program = optimizedProgram } 
    } where 
        body = Module.body modul
        g = callGraph modul
        optimizedProgram = tceTopLevelExpr (Graph.scc g) . Module.program $ body

tceTopLevelExpr :: [StronglyConnComp] -> Analyzed.Expr Info -> Analyzed.Expr Info
tceTopLevelExpr g (A ann expr) = A ann $ case expr of
    E.Let defs bodyExpr -> 
        E.Let (map (tceDef g) defs) (tceTopLevelExpr g bodyExpr)
    
    _ -> error "Compile error: The top-level expression should be a let-expression."

tceDef :: [StronglyConnComp] -> Analyzed.Def Info -> Analyzed.Def Info
tceDef g (Analyzed.Definition annPat@(A _ pat) defExpr maybeType) = case pat of
    Pat.Var (Var.Analyzed _ var) -> 
        Analyzed.Definition annPat (tceExpr g var defExpr) maybeType
    _ -> Analyzed.Definition (tcePat g annPat) (tceTopLevelExpr g defExpr) maybeType  

tceExpr :: [StronglyConnComp] -> Var.VarId -> Analyzed.Expr Info -> Analyzed.Expr Info
tceExpr sccs var expr@(A ann expr') = case expr' of
    E.Literal lit -> done expr
 
    E.Var (Var.Analyzed _ var') ->
        let sameSCC var var' = isJust $ find (== var') <$> find (elem var) sccs
        in
            if var `sameSCC` var'
            then continue expr
            else done expr 

    E.Range lowExpr highExpr -> undefined
     
    E.ExplicitList exprs -> 
        if all noDependencies exprs
        then done expr
        else continue expr

    E.Binop binOpVar leftArgExpr rightArgExpr -> 
        undefined

    E.Lambda pat bodyExpr -> 
        if noDependencies bodyExpr
        then done expr
        else continue expr

    E.App appExpr argExpr -> 
        if noDependencies appExpr && noDependencies argExpr
        then done expr
        else continue expr

    E.MultiIf ifExprs -> A ann $
        -- Presupposes that this expression has already been checked and found tail-recursive.
        E.MultiIf (zip (map fst ifExprs) $ map (tceExpr sccs var . snd) ifExprs)

    E.Let defs bodyExpr -> A ann $ 
        (E.Let (map (tceDef sccs) defs) $ 
            if noDependencies bodyExpr
            then done expr
            else continue expr)

    E.Case targetExpr cases ->
        -- Presupposes that this expression has already been checked and found tail-recursive.
        let tceCase (casePat, caseExpr) = (tcePat sccs casePat, tceExpr sccs var caseExpr) 
        in A ann $ E.Case (tceExpr sccs var targetExpr) (map tceCase cases)

    E.Data _ exprs ->
        if all noDependencies exprs
        then done expr
        else continue expr

    E.Access recordExpr _ -> 
        done expr    

    E.Remove recordExpr _ -> 
        done expr    

    E.Insert recordExpr _ insertExpr -> 
        done expr    

    E.Modify recordExpr modifications -> 
        done expr
        
    E.Record record -> 
        done expr    

    E.Port _ -> 
        done expr    

    E.GLShader _ _ _ -> 
        done expr

continue :: Analyzed.Expr ann -> Analyzed.Expr ann
continue (A ann expr) = A ann $ 
    E.Data "Continue" $ [A ann $ E.Lambda (A ann Pat.Anything) (A ann expr)]

done :: Analyzed.Expr ann -> Analyzed.Expr ann 
done (A ann expr) = A ann $ E.Data "Done" [A ann expr]

noDependencies = undefined

tcePat = undefined 
