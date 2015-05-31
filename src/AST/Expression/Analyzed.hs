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
  General.Expr ann (Def ann) Var.Analyzed Type.Canonical

type Expr' ann =
  General.Expr' ann (Def ann) Var.Analyzed Type.Canonical

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
