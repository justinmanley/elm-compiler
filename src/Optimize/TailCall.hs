module Optimize.TailCall where

import AST.Expression.General
import qualified AST.Expression.Canonical as Canonical
import qualified Language.ECMAScript3.Syntax as JavaScript
import Generate.JavaScript.Helpers (var, function, ret)
import qualified AST.Pattern as Pat
import Reporting.Annotation (Annotated(A)) 

tailCallOptimize :: Canonical.Def -> Maybe (JavaScript.Expression ())
tailCallOptimize (Canonical.Definition (A _ pat) expr _) = case pat of
    Pat.Var name -> 
        let (baseCases, recursiveCases) = analyzeCases name expr
            args = undefined
            body = 
                [ JavaScript.VarDeclStmt () $
                    [ JavaScript.VarDecl () (var "stack") (Just $ JavaScript.ArrayLit () []) ]
                , JavaScript.WhileStmt () undefined undefined
                , JavaScript.WhileStmt () undefined undefined
                , ret undefined
                ]
        in Just $ JavaScript.FuncExpr () (Just $ var name) args body
    _ -> Nothing
        
analyzeCases :: String
    -> Canonical.Expr 
    -> ([(Canonical.Expr, Canonical.Expr)], [(Canonical.Expr, Canonical.Expr, Canonical.Expr)])
analyzeCases name expr = undefined 
