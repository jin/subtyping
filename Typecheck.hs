module Typecheck where

import Syntax

isSubtype :: Ty -> Ty -> Bool
-- Function subtyping
isSubtype (ArrowTy a b) (ArrowTy x y) = isSubtype x a && isSubtype b y
-- Record subtyping
isSubtype (RcdTy xs) (RcdTy ys) = all sub ys
  where sub (lbl, ty) = case lookup lbl xs of
          Just ty2 -> isSubtype ty2 ty
          Nothing -> False
isSubtype ty1 ty2 = ty1 == ty2

typecheck :: Expr -> Either String Ty
typecheck (I _ IntTy) = Right IntTy
typecheck (B _ BoolTy) = Right BoolTy 
typecheck _ = Left "Error" 
