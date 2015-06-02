module Test.Optimize.TailCall where

import Test.Framework

import Optimize.TailCall (eliminateTailCalls) 

tceTests :: Test
tceTests = testGroup "tce tests" []
