{-# OPTIONS_GHC -Wall #-}
module Generate.Substitute (subst) where

import Control.Arrow (second, (***))

import AST.Expression.General (Expr'(..), PortImpl(..))
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as Pattern
import qualified AST.Variable as V
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A


subst :: V.CanonicalLocal -> Canonical.Expr' -> Canonical.Expr' -> Canonical.Expr'
subst old new expression =
    let f (A.A a e) = A.A a (subst old new e) in
    case expression of
      Range e1 e2 ->
          Range (f e1) (f e2)

      ExplicitList exprs ->
          ExplicitList (map f exprs)

      Binop op e1 e2 ->
          Binop op (f e1) (f e2)

      Lambda pattern body ->
          if Pattern.member old pattern
            then expression
            else Lambda pattern (f body)

      App e1 e2 ->
          App (f e1) (f e2)

      MultiIf branches ->
          MultiIf (map (f *** f) branches)

      Let defs body ->
          if anyShadow
            then expression
            else Let (map substDef defs) (f body)
        where
          substDef (Canonical.Definition p e t) =
              Canonical.Definition p (f e) t

          anyShadow =
              map (\(Canonical.Definition p _ _) -> p) defs
                |> any (Pattern.member old)

      Var var -> 
          case var of 
            V.Canonical V.Local _ -> 
                if var == old then new else expression

            _ -> expression
      Case e cases ->
          Case (f e) (map substCase cases)
        where
          substCase (pattern, expr) =
              if Pattern.member old pattern
                then (pattern, expr)
                else (pattern, f expr)

      Data tag values ->
          Data tag (map f values)

      Access record field ->
          Access (f record) field

      Remove record field ->
          Remove (f record) field

      Insert record field value ->
          Insert (f record) field (f value)

      Modify record fields ->
          Modify (f record) (map (second f) fields)

      Record fields ->
          Record (map (second f) fields)

      Literal _ ->
          expression

      GLShader _ _ _ ->
          expression

      Port impl ->
          Port $
            case impl of
              In _ _ ->
                  impl

              Out name expr tipe ->
                  Out name (f expr) tipe

              Task name expr tipe ->
                  Task name (f expr) tipe
