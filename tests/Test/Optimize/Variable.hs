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
    vars = Analyzed.variables . Module.program . Module.body . Var.uniquify $ modul

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
 

