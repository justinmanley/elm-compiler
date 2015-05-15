{-# LANGUAGE NamedFieldPuns #-}

module Optimize.Environment where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type UniqueVar = Int 

data Environment a = Environment
    { inScope :: Map a [UniqueVar]
    , varMap :: Map UniqueVar a
    , maxIndex :: UniqueVar }

fresh :: Ord a => Environment a -> a -> (Environment a, UniqueVar)
fresh (Environment { inScope, varMap, maxIndex }) var = 
    (Environment 
        { inScope = Map.insertWith (flip (++)) var [maxIndex] inScope
        , varMap = Map.insert maxIndex var varMap
        , maxIndex = maxIndex + 1 }
    , maxIndex)

empty :: Environment a
empty = Environment 
    { inScope = Map.empty
    , varMap = Map.empty
    , maxIndex = 0 }

-- If the name has already been seen, get the integer identifier in scope.
-- Otherwise, generate a fresh identifier for the name and store it.
variable :: Ord a => Environment a -> a -> (Environment a, UniqueVar)
variable env var = case Map.lookup var $ inScope env of
    Just vars -> case vars of
        [] -> fresh env var
        (v:vs) -> (env, v)
    Nothing -> fresh env var