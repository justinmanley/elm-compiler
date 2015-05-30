module Optimize.Info where

import Data.Set (Set)

import qualified AST.Variable as Var

data Info = Info 
    { role :: RecursionRole
    , contains :: Set Var.Analyzed
    } deriving Show

data RecursionRole
    = BaseCase
    | TailRecursiveCase
    deriving (Show, Eq)
