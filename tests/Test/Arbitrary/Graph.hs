module Test.Arbitrary.Graph where

import Data.Graph.Inductive.Graph
import Control.Applicative ((<$>), (<*>))
import Data.Traversable (forM)
import Control.Monad (replicateM)

import Test.QuickCheck

newtype SimpleGraph gr a b 
    = SimpleGraph (gr a b)
    deriving Show

instance (DynGraph gr, Arbitrary a, Arbitrary b) => Arbitrary (SimpleGraph gr a b) where
    arbitrary = SimpleGraph <$> (randomGraph =<< arbitrary)

-- | Generate a random simple digraph (that is, graphs with at most one directed edge for 
--   each pair of vertices and no self-loops), according to the Erdos-Renyi model.
--   The density parameter must be between 0 and 1.
randomGraph :: (DynGraph gr, Arbitrary a, Arbitrary b) 
    => Double 
    -> Gen (gr a b)
randomGraph density = sized $ \n -> 
    let nodes = [1..n]
        lnodes = zip nodes <$> arbitrary

        possibleEdges = fmap concat $ forM nodes $ \source -> 
            let targets = [1..source - 1] ++ [source + 1..n] 
            in zip3 (replicate (n - 1) source) targets <$> arbitrary

        selectEdges :: [LEdge b] -> Gen [LEdge b]
        selectEdges xs = (map fst . filter ((< density) . snd) . zip xs) <$> edgeProbabilities  
        
        edgeProbabilities :: Gen [Double]
        edgeProbabilities = replicateM n $ choose (0,1)
        
        edges = selectEdges =<< possibleEdges
    in mkGraph <$> lnodes <*> edges
    