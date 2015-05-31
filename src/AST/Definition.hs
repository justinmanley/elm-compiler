module AST.Definition where

import Text.PrettyPrint as P

import qualified AST.Type as Type
import qualified Reporting.PrettyPrint as P

data Definition pat expr = Definition pat expr (Maybe Type.Canonical)
    deriving Show

instance (P.Pretty pat, P.Pretty expr) => P.Pretty (Definition pat expr) where
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
