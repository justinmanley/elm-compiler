module Test.Optimize.CallGraph where

import qualified Data.Map as Map
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Control.Monad.State (State, evalState, runState, execState)
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Basic (hasLoop)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intersect)
import Data.Ix (inRange)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Arbitrary)

import Test.Utils (testModule)
import Test.Arbitrary.Graph
import Test.Arbitrary.CallGraph

import Optimize.CallGraph
import Optimize.Variable (uniquify)
import qualified Compile
import qualified AST.Module as Module
import Reporting.Error (Error)
import qualified Reporting.Error as Error
import Reporting.Warning (Warning)
import Reporting.Result (Result(Result), RawResult(Ok, Err))
import qualified AST.Variable as Var
import Reporting.PrettyPrint (pretty)

type DependencyGr = SimpleGraph Gr () Dependency

callGraphTests :: Test
callGraphTests = testGroup "Call graph construction" $ 
    [ buildTest $ testModule treeTest "Tree.elm" 
    , buildTest $ testModule treeTest "TreeRecord.elm" 
    , buildTest $ testModule idTest ("Soundness" </> "Id.elm") 
    , testProperty "mergeWith identity" mergeWithIdentity
    , testProperty "mergeWith body" mergeWithBody
    , testProperty "mergeWith doesNotOccur" mergeWithDoesNotOccur
    , testProperty "mergeWith fixBody" mergeWithFixBody
    , testProperty "updateEdge adds at most one edge" updateEdgeTest
    , testProperty "fixBody" fixBodyTest
    ]

mergeWithIdentity :: DependencyGr -> DependencyGr -> Bool
mergeWithIdentity (SimpleGraph g1) (SimpleGraph g2) = Graph.equal merged g1 where
    merged = g1 `ignore` g2
    ignore = mergeWith $ \edge -> id

mergeWithBody :: DependencyGr -> Bool
mergeWithBody (SimpleGraph g) = all isBody $ Graph.labEdges merged where
    merged = mergeWith body Graph.empty g

mergeWithDoesNotOccur :: DependencyGr -> Bool
mergeWithDoesNotOccur (SimpleGraph g) = all isBody $ Graph.labEdges merged where
    merged = mergeWith doesNotOccur g g

mergeWithFixBody :: DependencyGr -> Bool
mergeWithFixBody (SimpleGraph g) = all isBody $ Graph.labEdges merged where
    merged = mergeWith fixBody (Graph.emap swapDependency g) g

fixBodyTest :: DependencyGr -> Bool
fixBodyTest (SimpleGraph g) = case Graph.labEdges g of
    []      -> True
    ledge:_ -> 
        let esize = length $ Graph.labEdges g
            g' = fixBody ledge g
        in (length $ Graph.labEdges g') == esize

swapDependency :: Dependency -> Dependency
swapDependency dep = case dep of
    Body -> Tail
    Tail -> Body

-- | Helper function for mergeWith tests.
isBody :: Graph.LEdge Dependency -> Bool
isBody (_, _, dependency) = case dependency of
    Body -> True
    _    -> False

updateEdgeTest :: Graph.LEdge Dependency -> SimpleGraph Gr () Dependency -> Bool
updateEdgeTest ledge (SimpleGraph g) = inRange (size, size + 1) updatedSize where
    size = length . Graph.labEdges $ g
    updatedSize = length . Graph.labEdges $ updateEdge ledge g

treeTest :: Module.CanonicalModule -> Assertion
treeTest modul = assertBool failMessage (hasLoop treeCallGraph) where
    treeCallGraph = callGraph . uniquify $ modul
    failMessage = unlines $
        [ "recursive function should cause a loop in the call graph."
        , "final call graph: " ++ Graph.prettify treeCallGraph
        , show . pretty Map.empty False . Module.program . Module.body $ modul ] 

idTest :: Module.CanonicalModule -> Assertion
idTest modul = assertBool failMessage (not . hasLoop $ idCallGraph) where
    idCallGraph = callGraph . uniquify $ modul
    failMessage = unlines $
        [ "non-recursive function should result in a call graph without loops." 
        , Graph.prettify idCallGraph ]

testsDir :: FilePath
testsDir = "tests" </> "test-files" </> "good"


