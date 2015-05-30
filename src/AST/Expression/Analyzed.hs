module AST.Expression.Analyzed where

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.Info as Optimize
import qualified Reporting.Region as R

type Expr ann =
  General.Expr ann (Def ann) Var.Analyzed Type.Canonical

type Expr' ann =
  General.Expr' ann (Def ann) Var.Analyzed Type.Canonical

data Def ann = 
    Definition (Pattern.AnalyzedPattern ann) (Expr ann) (Maybe Type.Canonical)  
    deriving Show
