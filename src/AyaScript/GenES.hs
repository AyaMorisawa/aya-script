module AyaScript.GenES where

import AyaScript.Types
import Data.List

genES :: Program -> String
genES stmts =
  "{\"type\":\"Program\"," ++
   "\"body\":[" ++ (intercalate "," $ genESStmt <$> stmts) ++ "]," ++
   "\"sourceType\":\"script\"}"

genESStmt :: Stmt -> String
genESStmt (Expr e) =
  "{\"type\":\"ExpressionStatement\"," ++
   "\"expression\":" ++ genESExpr e ++ "}"

genESStmt (Decl e1 e2) =
  "{\"type\":\"VariableDeclaration\"," ++
   "\"declarations\":[{" ++
       "\"type\":\"VariableDeclarator\"," ++
       "\"id\":" ++ genESExpr e1 ++ "," ++
       "\"init\":" ++ genESExpr e2 ++ "}" ++
     "]," ++
   "\"kind\":\"var\"}"

genESStmt (Assign e1 e2) =
  "{\"type\":\"ExpressionStatement\"," ++
   "\"expression\":{" ++
     "\"type\":\"AssignmentExpression\"," ++
     "\"operator\":\"=\"," ++
     "\"left\":" ++ genESExpr e1 ++ "," ++
     "\"right\":" ++ genESExpr e2 ++ "}}"

genESExpr :: Expr -> String
genESExpr (Natural x) =
  "{\"type\":\"Literal\"," ++
   "\"value\":" ++ show x ++ "," ++
   "\"raw\":\"" ++ show x ++ "\"}"

genESExpr (Str x) =
  "{\"type\":\"Literal\"," ++
   "\"value\":\"" ++ x ++ "\"," ++
   "\"raw\":\"'" ++ x ++ "'\"}"

genESExpr (UnaryOp op e) =
  "{\"type\":\"UnaryExpression\"," ++
   "\"operator\":\"" ++ op ++ "\"," ++
   "\"argument\":" ++ genESExpr e ++ "," ++
   "\"prefix\":true}"

genESExpr (BinOp "." e1 e2@(Var _)) =
  "{\"type\":\"MemberExpression\"," ++
   "\"computed\":false," ++
   "\"object\":" ++ genESExpr e1 ++ "," ++
   "\"property\":" ++ genESExpr e2 ++ "}"

genESExpr (BinOp "." e1 e2) =
  "{\"type\":\"MemberExpression\"," ++
   "\"computed\":true," ++
   "\"object\":" ++ genESExpr e1 ++ "," ++
   "\"property\":" ++ genESExpr e2 ++ "}"

genESExpr (BinOp "**" e1 e2) = genESExpr (BinOp "" (BinOp "." (Var "Math") (Var "pow")) (Tuple [e1, e2]))

genESExpr (BinOp "|>" x f) = genESExpr (BinOp "" f x)

genESExpr (BinOp "<$>" f xs) = genESExpr (BinOp "" (BinOp "." xs (Var "map")) f)

genESExpr (BinOp "==" e1 e2) = genESExpr (BinOp "===" e1 e2)

genESExpr (BinOp "/=" e1 e2) = genESExpr (BinOp "!==" e1 e2)

genESExpr (BinOp "" e1 (Tuple es)) =
  "{\"type\":\"CallExpression\"," ++
   "\"callee\":" ++ genESExpr e1 ++ "," ++
   "\"arguments\":[" ++ (intercalate "," $ genESExpr <$> es) ++ "]}"

genESExpr (BinOp "" e1 e2) =
  "{\"type\":\"CallExpression\"," ++
   "\"callee\":" ++ genESExpr e1 ++ "," ++
   "\"arguments\":[" ++ genESExpr e2 ++ "]}"

genESExpr (BinOp op e1 e2) =
  "{\"type\":\"BinaryExpression\"," ++
   "\"operator\":\"" ++ op ++ "\"," ++
   "\"left\":" ++ genESExpr e1 ++ "," ++
   "\"right\":" ++ genESExpr e2 ++ "}"

genESExpr (Var var) =
  "{\"type\":\"Identifier\"," ++
   "\"name\":\"" ++ var ++ "\"}"

genESExpr (Fun param e) =
  "{\"type\":\"ArrowFunctionExpression\"," ++
   "\"id\":null," ++
   "\"params\":[" ++ genESExpr (Var param) ++ "]," ++
   "\"defaults\":[]," ++
   "\"body\":" ++ genESExpr e ++ "," ++
   "\"generator\":false," ++
   "\"expression\":true}"

genESExpr (If e1 e2 e3) =
  "{\"type\":\"ConditionalExpression\"," ++
   "\"test\":" ++ genESExpr e1 ++ "," ++
   "\"consequent\":" ++ genESExpr e2 ++ "," ++
   "\"alternate\":" ++ genESExpr e3 ++ "}"

genESExpr (Tuple es) = genESExpr (List es)

genESExpr (List es) =
  "{\"type\":\"ArrayExpression\"," ++
   "\"elements\":[" ++ (intercalate "," $ genESExpr <$> es) ++ "]}"
