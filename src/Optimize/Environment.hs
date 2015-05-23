{-# LANGUAGE NamedFieldPuns #-}

module Optimize.Environment where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State (State, get, modify)
import Control.Applicative ((<$>))

type UniqueVar = Int 

data Environment a = Environment
    { inScope :: Map a [UniqueVar]
    , varMap :: Map UniqueVar a
    , maxIndex :: UniqueVar }
    deriving Show

freshVar :: Ord a => a -> State (Environment a) UniqueVar
freshVar var = do
    identifier <- maxIndex <$> get

    modify $ \(Environment { inScope, varMap, maxIndex }) -> Environment 
        { inScope = Map.insertWith (flip (++)) var [maxIndex] inScope
        , varMap = Map.insert maxIndex var varMap
        , maxIndex = maxIndex + 1 }

    return identifier

empty :: Environment a
empty = Environment 
    { inScope = Map.empty
    , varMap = Map.empty
    , maxIndex = 0 }

-- If the name has already been seen, get the integer identifier in scope.
-- Otherwise, generate a fresh identifier for the name and store it.
getVar :: Ord a => a -> State (Environment a) UniqueVar
getVar var = do
    env <- get

    case Map.lookup var $ inScope env of
        Just vars -> case vars of
            []     -> freshVar var
            (v:vs) -> return v
        Nothing   -> freshVar var
