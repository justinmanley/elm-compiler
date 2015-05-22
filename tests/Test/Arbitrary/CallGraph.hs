module Test.Arbitrary.CallGraph where

import Test.QuickCheck

import Optimize.CallGraph (Dependency(Body,Tail))

instance Arbitrary Dependency where
	arbitrary = elements [Body, Tail]
