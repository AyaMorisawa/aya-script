module AyaScript.Types where

type Program = [Stmt]

data Stmt = Expr Expr
          | Decl Expr Expr
          | Assign Expr Expr
  deriving (Show, Eq)

data Expr = Natural Integer
          | Str String
          | UnaryOp String Expr
          | BinOp String Expr Expr
          | Var String
          | Fun String Expr
          | If Expr Expr Expr
          | Tuple [Expr]
          | List [Expr]
  deriving (Show, Eq)
