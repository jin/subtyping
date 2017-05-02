-- Some function signatures are too cryptic, so we're leaving them out.
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parser where

import Prelude hiding (GT, LT)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Exp

import Lexer
import Syntax
import Error

basicType :: Parser Ty
basicType = do
  tyName <- identifier
  case tyName of
       "Int" -> return IntTy
       "Bool" -> return BoolTy
       _ -> errorUnknownType tyName

rcdType :: Parser Ty
rcdType = do
  reserved "{"
  fields <- sepBy rcdFieldType (spaces *> char ',' <* spaces)
  reserved "}"
  return $ RcdTy fields

rcdFieldType = do
  lbl <- identifier
  reserved ":"
  ty <- typeParsers
  return (lbl, ty)

basicTypes = basicType <|> rcdType

fnType :: Parser Ty
fnType = do
  ty1 <- try basicTypes <|> parens fnType
  reserved "->"
  ty2 <- try basicTypes <|> parens fnType
  return $ ArrowTy ty1 ty2

typeParsers :: Parser Ty
typeParsers = basicTypes <|> fnType

exprType :: Parser Ty
exprType = do
  reserved "::"
  typeParsers

intExpr :: Parser Expr
intExpr = do
  n <- integer
  return $ I (fromInteger n) IntTy -- n is an Integer, we want Int

varExpr :: Parser Expr
varExpr = do
  var <- identifier
  case var of
       "true" -> return $ B True BoolTy
       "false" -> return $ B False BoolTy
       v -> return $ Var v

-- Anonymous function
-- e.g. fn x => x + 1
fnExpr :: Parser Expr
fnExpr = do
  reserved "fn"
  var <- identifier
  reserved "::"
  ty <- parens fnType
  reserved "=>"
  body <- expr
  return $ Fn var body ty

-- Records
-- Nested records are permitted
-- e.g. { a = 2, foo = true, c = 4 + 2, d = { bar = false } }
rcdExpr :: Parser Expr
rcdExpr = do
  reserved "{"
  fields <- sepBy rcdField (spaces *> char ',' <* spaces)
  reserved "}"
  ty <- exprType
  return $ Rcd fields ty

rcdField = do
  lbl <- identifier
  reserved "="
  value <- expr
  return (lbl, value)

expr :: Parser Expr
expr = Exp.buildExpressionParser opTable exprParsers

-- The table of operations on expressions.
--
-- Parsec uses this table to take care of associativity and precedence automatically.
-- The table is ordered by descending precedence, where operators in the same row having the same precedence.
opTable = [[Exp.Infix spacef Exp.AssocLeft],
           [projectionOp "." Exp.AssocLeft]]
  where
    projectionOp s = Exp.Infix $ reservedOp s >> return RcdProj
    -- Treat spaces as a binary operator for function application
    -- http://stackoverflow.com/questions/22904287/parsing-functional-application-with-parsec
    spacef = whiteSpace
      *> notFollowedBy (choice . map reservedOp $ opNames)
      >> return FApp

exprParsers = varExpr
          <|> fnExpr
          <|> rcdExpr
          <|> intExpr
          <|> parens expr

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""
