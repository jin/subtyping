module Typecheck (typecheckExpr) where

import Error
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

typecheck :: TypeEnv -> Expr -> Either String Ty
typecheck _ (I _ IntTy) = Right IntTy
typecheck _ (B _ BoolTy) = Right BoolTy 
typecheck env (Var v) = case getType v env of
                           Just ty -> Right ty
                           Nothing -> Left $ "Variable " ++ v ++ " is not defined in the environment." 
typecheck env (Fn arg body ty) = 
  case ty of 
    ArrowTy ty1 ty2 -> let newEnv = bindType arg ty1 env in
                       case typecheck newEnv body of
                            Left err -> Left err
                            Right bodyTy -> if isSubtype bodyTy ty2
                                               then Right (ArrowTy ty1 ty2) 
                                               else Left $ mismatchMsg ty2 bodyTy
    t -> Left $ "Expected Arrow type, got " ++ show t
typecheck env (Rcd xs (RcdTy ty)) = if all check ty then Right (RcdTy ty) else Left "Incorrect record type"
  where check (lbl, t1) = case lookup lbl xs of
                             Just e -> case typecheck env e of 
                                            Left _ -> False
                                            Right t2 -> isSubtype t2 t1 
                             Nothing -> False
typecheck env (FApp fn arg) = 
  case typecheck env fn of 
       Left err -> Left err
       Right (ArrowTy t1 t2) ->
           case typecheck env arg of
                Left err -> Left err
                Right argType -> if isSubtype argType t1 then Right t2 else Left "Incorrect argument type."
       Right t -> Left $ "Expected Arrow type, got " ++ show t 
typecheck _ _ = Left "Error: Unknown type. TODO: elaborate" 

typecheckExpr :: Expr -> Either String Ty
typecheckExpr = typecheck emptyEnv
