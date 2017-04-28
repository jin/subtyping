module Syntax where

import Data.List (intercalate)

data Op = Add | Sub | Mul | Div | GTE | GT | LTE | LT | Equal deriving (Show, Eq)

data Ty = RcdTy [(String, Ty)]
        | ArrowTy Ty Ty
        -- | TopTy
        | IntTy
        | BoolTy
        deriving (Show, Eq)

prettyTy :: Ty -> String
prettyTy (RcdTy xs) = "{ " ++ prettyRcds xs ++ " }" 
  where prettyRcds = intercalate ", " . map (\(lbl, ty) -> lbl ++ ": " ++ prettyTy ty)
prettyTy (ArrowTy x1 x2) = prettyTy x1 ++ " -> " ++ prettyTy x2
prettyTy IntTy = "Int"
prettyTy BoolTy = "Bool"

type Name = String

data Expr =
  I Int Ty |
  -- S String |
  B Bool Ty |
  Var Name |
  Fn Name Expr Ty |
  -- Fun Name Name Expr |
  FApp Expr Expr |
  -- Cond Expr Expr Expr |
  -- Let Expr Expr Expr |
  BinOp Op Expr Expr |
  Rcd [(Name, Expr)] Ty |
  RcdProj Expr Expr
    deriving (Show, Eq)
