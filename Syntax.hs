module Syntax where

data Op = Add | Sub | Mul | Div | GTE | LTE | Equal deriving (Show, Eq)

type Name = String

data Expr =
  I Int |
  S String |
  B Bool |
  Var Name |
  Fn Name Expr |
  Fun Name Name Expr |
  FApp Expr Expr |
  Cond Expr Expr Expr |
  Let Name Expr Expr |
  BinOp Op Expr Expr
    deriving (Show, Eq)

