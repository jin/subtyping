module Typecheck where

import Syntax
import Data.Map (Map)
import qualified Data.Map as Map

-- A type environment TypeEnv is a mapping from a program variable to a type.
newtype TypeEnv = TypeEnv (Map String Ty) deriving Show

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

bindType :: String -> Ty -> TypeEnv -> TypeEnv
bindType label t (TypeEnv env) = TypeEnv (Map.insert label t env)

getType :: String -> TypeEnv -> Maybe Ty
getType label (TypeEnv env) = Map.lookup label env

-- A function to check if one type is a subtype of the other.
isSubtype :: Ty -> Ty -> Bool
isSubtype (ArrowTy a b) (ArrowTy x y) = isSubtype x a && isSubtype b y -- Function subtyping
isSubtype (RcdTy xs) (RcdTy ys) = all sub ys -- Record subtyping
  where sub (lbl, ty) = case lookup lbl xs of
                             Just ty2 -> isSubtype ty2 ty
                             Nothing -> False
isSubtype ty1 ty2 = ty1 == ty2

-- infer env (FApp e1 e2) = do
--   (t1, s1) <- infer env e1
--   (t2, s2) <- infer (substEnv s1 env) e2
--   fVar     <- fresh
--   s3       <- unify (substType s2 t1) (ArrowT t2 fVar)
--   return (substType s3 fVar, s3 ++ s2 ++ s1)

typecheck :: TypeEnv -> Expr -> Either String Ty
typecheck _ (I _ IntTy) = Right IntTy
typecheck _ (B _ BoolTy) = Right BoolTy 
typecheck env (Var v) = case getType v env of
                             Just ty -> Right ty
                             Nothing -> Left $ "Variable " ++ v ++ " is not defined in the environment." 
typecheck env (Fn arg body (ArrowTy ty1 ty2)) = 
  let newEnv = bindType arg ty1 env in
  case typecheck newEnv body of
       Left err -> Left err
       Right bodyTy -> 
         if ty2 == bodyTy then Right (ArrowTy ty1 ty2) else Left "Incorrect function type. TODO: elaborate"
typecheck _ _ = Left "Error" 
