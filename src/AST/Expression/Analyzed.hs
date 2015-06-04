module AST.Expression.Analyzed where

import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.Info as Optimize
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R

type Expr ann =
  General.Expr ann (Def ann) Var.Analyzed Var.Analyzed Type.Canonical

type Expr' ann =
  General.Expr' ann (Def ann) Var.Analyzed Var.Analyzed Type.Canonical

data Def ann = 
    Definition (Pattern.AnalyzedPattern ann) (Expr ann) (Maybe (A.Located Type.Canonical))  
    deriving Show

-- Copied directly from the P.Pretty instance for Canonical.Def.
-- Is there a way of giving this instance declaration without the code duplication?
instance P.Pretty (Def ann) where
  pretty dealiaser _ (Definition pattern expr maybeTipe) =
      P.vcat [ annotation, definition ]
    where
      definition =
          P.pretty dealiaser True pattern
          <+> P.equals
          <+> P.pretty dealiaser False expr

      annotation =
          case maybeTipe of
            Nothing ->
                P.empty

            Just tipe ->
                P.pretty dealiaser True pattern
                <+> P.colon
                <+> P.pretty dealiaser False tipe


variables :: Expr ann -> [Var.Analyzed]
variables (A.A _ expr) = case expr of
    General.Literal _ -> 
        [] 
    
    General.Var var -> 
        [var]
    
    General.Range lowExpr highExpr -> 
        variables lowExpr ++ variables highExpr
    
    General.ExplicitList exprs -> 
        concatMap variables  exprs
    
    General.Binop binOpVar leftArgExpr rightArgExpr -> 
        binOpVar : (variables leftArgExpr ++ variables rightArgExpr)
    
    General.Lambda pat expr' -> 
        variablesInPat pat ++ variables expr'
    
    General.App appExpr argExpr -> 
        variables appExpr ++ variables argExpr
    
    General.MultiIf ifExprs -> 
        let variablesInIfExpr (ifExpr, thenExpr) = 
                variables ifExpr ++ variables thenExpr
        in concatMap variablesInIfExpr ifExprs
    
    General.Let defs bodyExpr ->
        concatMap variablesInDef defs ++ variables bodyExpr
    
    General.Case targetExpr cases ->
        let variablesInCase (casePat, caseExpr) = 
                variablesInPat casePat ++ variables caseExpr
        in variables targetExpr ++ concatMap variablesInCase cases
    
    General.Data _ exprs ->
        concatMap variables exprs        
    
    General.Access recordExpr _ -> 
        variables recordExpr

    General.Remove recordExpr _ -> 
        variables recordExpr    

    General.Insert recordExpr _ insertExpr -> 
        variables recordExpr ++ variables insertExpr    

    General.Modify recordExpr modifications -> 
        variables recordExpr ++ concatMap (variables . snd) modifications    

    General.Record record -> 
        concatMap (variables . snd) record   
 
    General.Port portImpl -> case portImpl of
        General.In _ _ -> []
        General.Out _ outExpr _ -> variables outExpr
        General.Task _ taskExpr _ -> variables taskExpr   

    General.GLShader _ _ _ -> 
        []

variablesInDef :: Def ann -> [Var.Analyzed]
variablesInDef (Definition pat expr _) = 
    variablesInPat pat ++ variables expr  

variablesInPat :: Pattern.AnalyzedPattern ann -> [Var.Analyzed]
variablesInPat (A.A _ pat) = case pat of
    Pattern.Data var pats -> var : (concatMap variablesInPat pats)
    
    Pattern.Alias _ aliasPat -> variablesInPat aliasPat

    _ -> []

saveEnvVar :: Var.Analyzed
saveEnvVar = Var.Analyzed (Var.builtin General.saveEnvName) 0
