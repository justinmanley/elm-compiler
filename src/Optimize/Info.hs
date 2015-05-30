module Optimize.Info where

import Data.Set (Set)

data Info = Info 
    { branchType :: InductiveType
    , contains :: Set String -- names referenced in the expression
    }

data InductiveType
    = BaseCase
    | TailRecursiveCase
