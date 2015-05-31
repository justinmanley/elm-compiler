{-# LANGUAGE NamedFieldPuns #-}

module Test.Optimize.Variable where

import qualified Data.Map as Map
import System.FilePath ((</>))

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool)

import qualified AST.Expression.Analyzed as Analyzed
import qualified AST.Expression.General as E
import qualified AST.Module as Module
import qualified AST.Pattern as Pat
import qualified AST.Variable as Var
import qualified Optimize.Variable as Var
import Reporting.Annotation ( Annotated(A) )
import Reporting.PrettyPrint (pretty)

import Test.Utils (testModule, prettyExpr)

uniquifyTests :: Test
uniquifyTests = testGroup "Distinct canonical vars should have distinct ids" $ 
    map (buildTest . testModule idsAreUnique) 
        [ "Soundness" </> "Id.elm"
        , "ExplicitList.elm"
        , "Defs.elm" ]

idsAreUnique :: Module.CanonicalModule -> Assertion
idsAreUnique modul = either failure success $ foldr isUnique (Right Map.empty) vars where
    failure msg = assertFailure $ unlines [ msg, prettyExpr . Var.uniquify $ modul ]
    success _ = assertBool "" True
    vars = variables . Module.program . Module.body . Var.uniquify $ modul

isUnique :: Var.Analyzed 
    -> Either String (Map.Map Int Var.Canonical)
    -> Either String (Map.Map Int Var.Canonical)
isUnique var state = state >>= \ids -> 
    case Map.lookup (Var.varId var) ids of
        Nothing -> Right $ Map.insert (Var.varId var) (Var.canonicalName var) ids
        Just canonicalName' -> 
            if Var.canonicalName var == canonicalName'
            then Right ids
            else Left $ unlines 
                [ show (Var.canonicalName var) ++ " and " ++ show canonicalName' 
                , " are both assigned to the same variable id: "
                , show (Var.varId var) ++ "." ]
 
variables :: Analyzed.Expr ann -> [Var.Analyzed]
variables (A _ expr) = case expr of
    E.Literal _ -> 
        [] 
    
    E.Var var -> 
        [var]
    
    E.Range lowExpr highExpr -> 
        variables lowExpr ++ variables highExpr
    
    E.ExplicitList exprs -> 
        concatMap variables  exprs
    
    E.Binop binOpVar leftArgExpr rightArgExpr -> 
        binOpVar : (variables leftArgExpr ++ variables rightArgExpr)
    
    E.Lambda pat expr' -> 
        variablesInPat pat ++ variables expr'
    
    E.App appExpr argExpr -> 
        variables appExpr ++ variables argExpr
    
    E.MultiIf ifExprs -> 
        let variablesInIfExpr (ifExpr, thenExpr) = 
                variables ifExpr ++ variables thenExpr
        in concatMap variablesInIfExpr ifExprs
    
    E.Let defs bodyExpr ->
        concatMap variablesInDef defs ++ variables bodyExpr
    
    E.Case targetExpr cases ->
        let variablesInCase (casePat, caseExpr) = 
                variablesInPat casePat ++ variables caseExpr
        in variables targetExpr ++ concatMap variablesInCase cases
    
    E.Data _ exprs ->
        concatMap variables exprs        
    
    E.Access recordExpr _ -> 
        variables recordExpr

    E.Remove recordExpr _ -> 
        variables recordExpr    

    E.Insert recordExpr _ insertExpr -> 
        variables recordExpr ++ variables insertExpr    

    E.Modify recordExpr modifications -> 
        variables recordExpr ++ concatMap (variables . snd) modifications    

    E.Record record -> 
        concatMap (variables . snd) record   
 
    E.Port portImpl -> case portImpl of
        E.In _ _ -> []
        E.Out _ outExpr _ -> variables outExpr
        E.Task _ taskExpr _ -> variables taskExpr   

    E.GLShader _ _ _ -> 
        []

variablesInDef :: Analyzed.Def ann -> [Var.Analyzed]
variablesInDef (Analyzed.Definition pat expr _) = 
    variablesInPat pat ++ variables expr  

variablesInPat :: Pat.AnalyzedPattern ann -> [Var.Analyzed]
variablesInPat (A _ pat) = case pat of
    Pat.Data var pats -> var : (concatMap variablesInPat pats)
    
    Pat.Alias _ aliasPat -> variablesInPat aliasPat

    _ -> [] 
