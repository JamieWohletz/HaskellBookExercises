module HuttonsRazor where

data Expr = Lit Integer | Add Expr Expr

-- 1
eval :: Expr -> Integer
eval (Lit x) = x
eval (Add exp1 exp2) = eval exp1 + eval exp2

-- 2
printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add exp1 exp2) = printExpr exp1 ++ " + " ++ printExpr exp2
