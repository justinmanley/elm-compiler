module Test.Optimize.CallGraph where

import qualified Data.Map as Map
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Control.Monad.State (evalState, runState)
import Data.Graph.Inductive.Graph (prettify)
import Data.Graph.Inductive.Basic (hasLoop)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool)

import Optimize.CallGraph
import qualified Optimize.Environment as Env
import qualified Compile
import qualified AST.Module as Module
import Reporting.Error (Error)
import qualified Reporting.Error as Error
import Reporting.Warning (Warning)
import Reporting.Result (Result(Result), RawResult(Ok, Err))

callGraphTests :: [Test]
callGraphTests = map buildTest 
    [ testModule "Tree.elm" treeTest 
    , testModule ("Soundness" </> "Id.elm") idTest ]

treeTest :: Module.CanonicalModule -> Assertion
treeTest modul = assertBool failMessage (hasLoop treeCallGraph) where
    (treeCallGraph, env) = runState (callGraph modul) Env.empty :: (DependencyGraph Gr, VarEnv)
    failMessage = unlines $
        [ "recursive function should cause a loop in the call graph."
        , prettify treeCallGraph
        , show env ] 

idTest :: Module.CanonicalModule -> Assertion
idTest modul = assertBool failMessage (not . hasLoop $ idCallGraph) where
    idCallGraph = evalState (callGraph modul) Env.empty :: DependencyGraph Gr
    failMessage = unlines $
        [ "non-recursive function should result in a call graph without loops." 
        , prettify idCallGraph ]

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
        Err errors -> assertFailure $ concatMap errorToString  errors

