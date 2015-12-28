{-# LANGUAGE OverloadedStrings #-}

module AyaScript.GenES where

import AyaScript.Types
import Data.List
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)

s :: String -> String
s = id

n :: Num a => a -> a
n = id

me :: Maybe Expr -> Maybe Expr
me = id

le :: [Expr] -> [Expr]
le = id

o :: ToJSON a => a -> String
o = unpack . encode

genES :: Program -> String
genES stmts = o $ object [ "type" .= s "Program"
                         , "body" .= stmts
                         , "sourceType" .= s "script"]

instance ToJSON Expr where
  toJSON (Natural x) = object [ "type" .= s "Literal"
                              , "value" .= n x
                              , "raw" .= show x]

  toJSON (Str x) = object [ "type" .= s "Literal"
                          , "value" .= x
                          , "raw" .= s ("'" ++ x ++ "'")]

  toJSON (UnaryOp op e) = object [ "type" .= s "UnaryExpression"
                               , "operator" .= op
                               , "argument" .= e
                               , "prefix" .= True]

  toJSON (BinOp "." e1 e2@(Var _)) = object [ "type" .= s "MemberExpression"
                                            , "computed" .= False
                                            , "object" .= e1
                                            , "property" .= e2]

  toJSON (BinOp "." e1 e2) = object [ "type" .= s "MemberExpression"
                                    , "computed" .= True
                                    , "object" .= e1
                                    , "property" .= e2]

  toJSON (BinOp "**" e1 e2) = toJSON $ BinOp "" (BinOp "." (Var "Math") (Var "pow")) (Tuple [e1, e2])

  toJSON (BinOp "|>" x f) = toJSON $ BinOp "" f x

  toJSON (BinOp "<$>" f xs) = toJSON $ BinOp "" (BinOp "." xs (Var "map")) f

  toJSON (BinOp "==" e1 e2) = toJSON $ BinOp "===" e1 e2

  toJSON (BinOp "/=" e1 e2) = toJSON $ BinOp "!==" e1 e2

  toJSON (BinOp "" e1 (Tuple es)) = object [ "type" .= s "CallExpression"
                                           , "callee" .= e1
                                           , "arguments" .= es]

  toJSON (BinOp "" e1 e2) = object [ "type" .= s "CallExpression"
                                   , "callee" .= e1
                                   , "arguments" .= [e2]]

  toJSON (BinOp op e1 e2) = object [ "type" .= s "BinaryExpression"
                                   , "operator" .= op
                                   , "left" .= e1
                                   , "right" .= e2]

  toJSON (Var var) = object [ "type" .= s "Identifier"
                            , "name" .= var]

  toJSON (Fun param e) = object [ "type" .= s "ArrowFunctionExpression"
                                , "id" .= me Nothing
                                , "params" .= [Var param]
                                , "defaults" .= le []
                                , "body" .= e
                                , "generator" .= False
                                , "expression" .= True]

  toJSON (If e1 e2 e3) = object [ "type" .= s "ConditionalExpression"
                                , "test" .= e1
                                , "consequent" .= e2
                                , "alternate" .= e3]

  toJSON (Tuple es) = toJSON $ List es

  toJSON (List es) = object [ "type" .= s "ArrayExpression"
                            , "elements" .= es]

instance ToJSON Stmt where
  toJSON (Expr e) = object [ "type" .= s "ExpressionStatement"
                           , "expression" .= e]

  toJSON (Decl e1 e2) = object [ "type" .= s "VariableDeclaration"
                               , "declarations" .= [object [ "type" .= s "VariableDeclarator"
                                                          , "id" .= e1
                                                          , "init" .= e2]]
                               , "kind" .= s "var"]

  toJSON (Assign e1 e2) = object [ "type" .= s "ExpressionStatement"
                                 , "expression" .= object [ "type" .= s "AssignmentExpression"
                                                          , "operator" .= s "="
                                                          , "left" .= e1
                                                          , "right" .= e2]]
