module Optimize.TailCall where

import Data.Graph.Inductive (DynGraph)

import qualified AST.Expression.Analyzed as Analyzed
import qualified AST.Module as Module
import qualified AST.Pattern as Pat
import qualified AST.Variable as Var
import Optimize.CallGraph (DependencyGraph, callGraph) 
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import Reporting.Annotation ( Annotated(A) )
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R

sameStronglyConnectedComponent = undefined

type Info = (R.Region, [Var.Analyzed])

eliminateTailCalls :: Module.AnalyzedModule Info -> Module.AnalyzedModule Info
eliminateTailCalls modul = modul { Module.body = 
        body { Module.program = optimizedProgram } 
    } where 
        body = Module.body modul
        g = callGraph modul
        optimizedProgram = tceTopLevelExpr g . Module.program $ body

tceTopLevelExpr :: DependencyGraph -> Analyzed.Expr Info -> Analyzed.Expr Info
tceTopLevelExpr g (A ann expr) = A ann $ case expr of
    E.Let defs bodyExpr -> 
        E.Let (map (tceDef g) defs) (tceTopLevelExpr g bodyExpr)
    
    _ -> error "Compile error: The top-level expression should be a let-expression."

tceDef :: DependencyGraph -> Analyzed.Def Info -> Analyzed.Def Info
tceDef g (Analyzed.Definition annPat@(A _ pat) defExpr maybeType) = case pat of
    Pat.Var str -> 
        let scc = undefined str -- strongly connected component containing the var.
        in Analyzed.Definition annPat (tceExpr scc defExpr) maybeType
   
    _ -> Analyzed.Definition (tcePat g annPat) (tceTopLevelExpr g defExpr) maybeType  

tceExpr :: DependencyGraph -> Analyzed.Expr Info -> Analyzed.Expr Info
tceExpr g expr@(A ann expr') = case expr' of
    E.Literal lit -> done expr
 
    E.Var var' ->
        let sameSCC = sameStronglyConnectedComponent g
        in
            if undefined --var `sameSCC` var'
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
        E.MultiIf (zip (map fst ifExprs) $ map (tceExpr g . snd) ifExprs)

    E.Let defs bodyExpr -> A ann $ 
        (E.Let (map (tceDef g) defs) $ 
            if noDependencies bodyExpr
            then done expr
            else continue expr)

    E.Case targetExpr cases ->
        -- Presupposes that this expression has already been checked and found tail-recursive.
        let tceCase (casePat, caseExpr) = (tcePat g casePat, tceExpr g caseExpr) 
        in A ann $ E.Case (tceExpr g targetExpr) (map tceCase cases)

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
