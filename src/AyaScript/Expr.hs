module AyaScript.Expr where

data Expr = Natural Integer
          | Str String
          | UnaryOp String Expr
          | BinOp String Expr Expr
          | Var String
          | Fun String Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Tuple [Expr]
          | List [Expr]
  deriving (Show, Eq)
