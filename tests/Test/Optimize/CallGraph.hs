module Test.Optimize.CallGraph where

import qualified Data.Map as Map
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Control.Monad.State (evalState)
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
callGraphTests = [ buildTest $ getModule "Tree.elm" treeTest ]

treeTest :: Module.CanonicalModule -> Assertion
treeTest modul = assertBool failMessage (hasLoop treeCallGraph) where
    treeCallGraph = evalState (callGraph modul) Env.empty :: DependencyGraph Gr
    failMessage = unlines $
        [ "recursive function should yield a loop in call graph: "
        , prettify treeCallGraph ] 

testsDir :: FilePath
testsDir = "tests/test-files/good"

getModule :: FilePath
    -> (Module.CanonicalModule -> Assertion)
    -> IO Test
getModule filePath testModule = do
    sourceCode <- readFile $ testsDir </> filePath

    let Result _ result = Compile.compile "elm-lang" "core" True Map.empty sourceCode
    let errorToString = Error.toString filePath sourceCode

    return $ testCase (testsDir </> filePath) $ case result of 
        Ok modul   -> testModule modul
        Err errors -> assertFailure $ concatMap errorToString  errors

