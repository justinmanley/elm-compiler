{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import qualified Data.Set as Set
import Text.PrettyPrint as P

import qualified AST.Helpers as Help
import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


type Pattern ann var local =
    A.Annotated ann (Pattern' ann var local)


data Pattern' ann var local
    = Data var [Pattern ann var local]
    | Record [local]
    | Alias local (Pattern ann var local)
    | Var local
    | Anything
    | Literal L.Literal
    deriving (Show)


type RawPattern =
    Pattern R.Region Var.Raw String


type RawPattern' =
    Pattern' R.Region Var.Raw String


type CanonicalPattern =
    Pattern R.Region Var.Canonical String


list :: R.Position -> [RawPattern] -> RawPattern
list end patterns =
  case patterns of
    [] ->
        A.at end end (Data (Var.Raw "[]") [])

    pattern@(A.A (R.Region start _) _) : rest ->
        A.at start end (Data (Var.Raw "::") [pattern, list end rest])


consMany :: R.Position -> [RawPattern] -> RawPattern
consMany end patterns =
  let cons hd@(A.A (R.Region start _) _) tl =
          A.at start end (Data (Var.Raw "::") [hd, tl])
  in
      foldr1 cons patterns


tuple :: [RawPattern] -> RawPattern'
tuple patterns =
  Data (Var.Raw ("_Tuple" ++ show (length patterns))) patterns


-- FIND VARIABLES

boundVars :: Pattern ann var String -> [A.Annotated ann String]
boundVars (A.A ann pattern) =
  case pattern of
    Var x ->
        [A.A ann x]

    Alias name realPattern ->
        A.A ann name : boundVars realPattern

    Data _ patterns ->
        concatMap boundVars patterns

    Record fields ->
        map (A.A ann) fields

    Anything ->
        []

    Literal _ ->
        []


member :: String -> Pattern ann var String -> Bool
member name pattern =
  any (name==) (map A.drop (boundVars pattern))


boundVarSet :: Pattern ann var String -> Set.Set String
boundVarSet pattern =
  Set.fromList (map A.drop (boundVars pattern))


boundVarList :: Pattern ann var String -> [String]
boundVarList pattern =
  Set.toList (boundVarSet pattern)


-- PRETTY PRINTING

instance (Var.ToString var, Var.ToString local) => P.Pretty (Pattern' ann var local) where
  pretty dealiaser needsParens pattern =
    case pattern of
      Var name ->
          P.text $ Var.toString name

      Literal literal ->
          P.pretty dealiaser needsParens literal

      Record fields ->
          P.braces (P.commaCat (map (P.text . Var.toString) fields))

      Alias x ptrn ->
          P.parensIf needsParens $
              P.pretty dealiaser True ptrn <+> P.text "as" <+> P.text (Var.toString x)

      Anything ->
          P.text "_"

      Data name [A.A _ hd, A.A _ tl]
          | Var.toString name == "::" ->
              P.parensIf isCons (P.pretty dealiaser False hd)
              <+> P.text "::"
              <+> P.pretty dealiaser False tl
          where
            isCons =
              case hd of
                Data ctor _ -> Var.toString ctor == "::"
                _ -> False

      Data name patterns ->
          let name' = Var.toString name
          in
            if Help.isTuple name'
              then
                P.parens (P.commaCat (map (P.pretty dealiaser False) patterns))
              else
                P.parensIf needsParens $
                    P.hsep (P.text name' : map (P.pretty dealiaser True) patterns)
