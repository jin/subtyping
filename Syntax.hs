module Syntax where

import Data.List (intercalate)

data Op = Add | Sub | Mul | Div | GTE | GT | LTE | LT | Equal deriving (Show, Eq)

data Ty = IntTy
        | BoolTy
        | ArrowTy Ty Ty
        | RcdTy [(String, Ty)]
          deriving (Show, Eq)

prettyTy :: Ty -> String
prettyTy (RcdTy xs) = "{ " ++ prettyRcds xs ++ " }"
  where prettyRcds = intercalate ", " . map (\(lbl, ty) -> lbl ++ ": " ++ prettyTy ty)
prettyTy (ArrowTy x1 x2) = "(" ++ prettyTy x1 ++ " -> " ++ prettyTy x2 ++ ")"
prettyTy IntTy = "Int"
prettyTy BoolTy = "Bool"

data Expr = I Int Ty
          | B Bool Ty
          | Var String
          | Fn String Expr Ty
          | FApp Expr Expr
          | BinOp Op Expr Expr
          | Rcd [(String, Expr)] Ty
          | RcdProj Expr Expr
            deriving (Show, Eq)
