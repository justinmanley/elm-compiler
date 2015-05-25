module Test.Optimize.CallGraph where

import qualified Data.Map as Map
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Control.Monad.State (State, evalState, runState, execState)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Basic (hasLoop)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intersect)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool)
import Test.Framework.Providers.QuickCheck2

import Test.Arbitrary.Graph
import Test.Arbitrary.CallGraph

import Optimize.CallGraph
import qualified Optimize.Environment as Env
import qualified Compile
import qualified AST.Module as Module
import Reporting.Error (Error)
import qualified Reporting.Error as Error
import Reporting.Warning (Warning)
import Reporting.Result (Result(Result), RawResult(Ok, Err))
import qualified AST.Variable as Var
import Reporting.PrettyPrint (pretty)

type DependencyGr = SimpleGraph Gr () Dependency

callGraphTests :: [Test]
callGraphTests = 
    [ buildTest $ testModule "Tree.elm" treeTest 
    , buildTest $ testModule ("Soundness" </> "Id.elm") idTest
    , buildTest envDefsTest
    , buildTest envNestedDefsTest
    , testProperty "mergeWith identity" mergeWithIdentity
    , testProperty "mergeWith body" mergeWithBody ]

envDefsTest :: IO Test
envDefsTest = testModule "Defs.elm" $ \modul -> 
    let env = execState (callGraph modul :: State VarEnv (DependencyGraph Gr)) Env.empty
        variables = 
            [ Var.Canonical { Var.home = Var.Local, Var.name = "a" }
            , Var.Canonical { Var.home = Var.Local, Var.name = "b" }
            , Var.Canonical { Var.home = Var.Local, Var.name = "c" } ]
        hasVariables = (Map.keys . Env.inScope $ env) == variables
        failMessage = unlines $ 
            [ "environment should contain bindings of all top-level names."
            , show . pretty False . Module.program . Module.body $ modul
            , show env ]
    in assertBool failMessage hasVariables 

envNestedDefsTest :: IO Test
envNestedDefsTest = testModule ("Soundness" </> "Apply.elm") $ \modul -> 
    let env = execState (callGraph modul :: State VarEnv (DependencyGraph Gr)) Env.empty
        variables = 
            [ Var.Canonical { Var.home = Var.Local, Var.name = "apply" }
            , Var.Canonical { Var.home = Var.Local, Var.name = "g" } ]
        hasVariables = (Map.keys . Env.inScope $ env) == variables
        failMessage = unlines $
            [ "environment should contain bindings of function names in nested scopes."
            , show env ]
    in assertBool failMessage hasVariables

mergeWithIdentity :: DependencyGr -> DependencyGr -> Bool
mergeWithIdentity (SimpleGraph g1) (SimpleGraph g2) = Graph.equal merged g1 where
    merged = g1 `ignore` g2
    ignore = mergeWith $ \edge -> id

mergeWithBody :: DependencyGr -> Bool
mergeWithBody (SimpleGraph g) = all isBody $ Graph.labEdges merged where
    isBody (_, _, dependency) = case dependency of
        Body -> True
        _    -> False
    merged = mergeWith body Graph.empty g

treeTest :: Module.CanonicalModule -> Assertion
treeTest modul = assertBool failMessage (hasLoop treeCallGraph) where
    (treeCallGraph, env) = runState (callGraph modul) Env.empty :: (DependencyGraph Gr, VarEnv)
    failMessage = unlines $
        [ "recursive function should cause a loop in the call graph."
        , Graph.prettify treeCallGraph
        , show . pretty False . Module.program . Module.body $ modul
        , show env ] 

idTest :: Module.CanonicalModule -> Assertion
idTest modul = assertBool failMessage (not . hasLoop $ idCallGraph) where
    idCallGraph = evalState (callGraph modul) Env.empty :: DependencyGraph Gr
    failMessage = unlines $
        [ "non-recursive function should result in a call graph without loops." 
        , Graph.prettify idCallGraph ]

testsDir :: FilePath
testsDir = "tests" </> "test-files" </> "good"

testModule :: FilePath
    -> (Module.CanonicalModule -> Assertion)
    -> IO Test
testModule filePath satisfiesProperty = do
    sourceCode <- readFile $ testsDir </> filePath
    let Result _ result = Compile.compile "elm-lang" "core" True Map.empty sourceCode
    let errorToString = Error.toString filePath sourceCode

    return $ testCase (testsDir </> filePath) $ case result of 
        Ok modul   -> satisfiesProperty modul
        Err errors -> assertFailure $ concatMap errorToString errors

