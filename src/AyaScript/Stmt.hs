module AyaScript.Stmt where

import AyaScript.Expr

data Stmt = Expr Expr
          | Decl Expr Expr
          | Assign Expr Expr
  deriving (Show, Eq)
