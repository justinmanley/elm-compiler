module Test.Utils where

import qualified Data.Map as Map
import System.FilePath ((</>))
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool)

import qualified AST.Module as Module
import qualified Compile
import qualified Reporting.Error as Error
import Reporting.PrettyPrint (Pretty, pretty)
import Reporting.Result (Result(Result), RawResult(Ok, Err))

testModule :: FilePath
    -> (Module.CanonicalModule -> Assertion)
    -> IO Test
testModule filePath satisfiesProperty = do
    sourceCode <- readFile $ testsDir </> filePath
    let Result _ result = Compile.compile "elm-lang" "core" True Map.empty sourceCode
    let errorToString = Error.toString Map.empty filePath sourceCode

    return $ testCase (testsDir </> filePath) $ case result of 
        Ok modul   -> satisfiesProperty modul
        Err errors -> assertFailure $ concatMap errorToString errors

testsDir :: FilePath
testsDir = "tests" </> "test-files" </> "good"

prettyExpr :: Pretty expr 
    => Module.Module imports exports (Module.ModuleBody expr) 
    -> String
prettyExpr = show . pretty Map.empty False . Module.program . Module.body
