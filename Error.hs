module Error where

import Syntax (Ty, prettyTy)

errorUnknownType :: String -> a
errorUnknownType tyName = error $ "Unknown type: " ++ tyName

errorTypeMismatch :: Ty -> Ty -> a
errorTypeMismatch expected actual = error $ mismatchMsg expected actual

mismatchMsg :: Ty -> Ty -> String
mismatchMsg expected actual = "Type mismatch: expected " ++ prettyTy expected ++ ", got " ++ prettyTy actual
