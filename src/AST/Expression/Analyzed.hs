module AST.Expression.Analyzed where

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.Info as Optimize
import qualified Reporting.Region as R

type Expr =
  General.Expr (R.Region, Optimize.Info) Canonical.Def Var.Analyzed Type.Canonical

type Expr' =
  General.Expr' (R.Region, Optimize.Info) Canonical.Def Var.Analyzed Type.Canonical

data Def = 
    Definition Pattern.AnalyzedPattern Expr (Maybe Type.Canonical)  
    deriving Show
