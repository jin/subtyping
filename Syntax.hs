module Syntax where

import Data.List (intercalate)

data Ty = IntTy
        | BoolTy
        | ArrowTy Ty Ty
        | RcdTy [(String, Ty)]
          deriving (Show, Eq)

data Expr = I Int Ty
          | B Bool Ty
          | Var String
          | Fn String Expr Ty
          | FApp Expr Expr
          | Rcd [(String, Expr)] Ty
          | RcdProj Expr Expr
            deriving (Show, Eq)

prettyTy :: Ty -> String
prettyTy (RcdTy xs) = "{ " ++ prettyRcds xs ++ " }"
  where prettyRcds = intercalate ", " . map (\(lbl, ty) -> lbl ++ ": " ++ prettyTy ty)
prettyTy (ArrowTy x1 x2) = "(" ++ prettyTy x1 ++ " -> " ++ prettyTy x2 ++ ")"
prettyTy IntTy = "Int"
prettyTy BoolTy = "Bool"
