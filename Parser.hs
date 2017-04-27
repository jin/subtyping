-- Some function signatures are too cryptic, so we're leaving them out.
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parser where

import Prelude hiding (GT, LT)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Exp

import Lexer
import Syntax

intExpr :: Parser Expr
intExpr = do
  n <- integer
  return $ I (fromInteger n) -- n is an Integer, we want Int

-- strExpr :: Parser Expr
-- strExpr = do
--   _ <- char '"'
--   x <- many (noneOf "\"")
--   _ <- char '"'
--   return $ S x

varExpr :: Parser Expr
varExpr = do
  var <- identifier
  -- return $ filterKeywords var
  return $ Var var
    -- where
      -- filterKeywords "true" = B True
      -- filterKeywords "false" = B False
      -- filterKeywords v = Var v

-- Let expression
-- e.g. let x = 3 in x + 2
-- letExpr :: Parser Expr
-- letExpr = do
--   reserved "let"
--   var <- varExpr
--   reserved "="
--   expr' <- expr
--   reserved "in"
--   body <- expr
--   return $ Let var expr' body

-- Anonymous function
-- e.g. fn x => x + 1
fnExpr :: Parser Expr
fnExpr = do
  reserved "fn"
  var <- identifier
  reserved "=>"
  body <- expr
  return $ Fn var body

-- Named function
-- e.g. fun f x y => x + y
-- funExpr :: Parser Expr
-- funExpr = do
--   reserved "fun"
--   name <- identifier
--   vars <- identifier
--   reserved "=>"
--   body <- expr
--   return $ Fun name vars body

-- Conditionals
-- e.g. if x >= y then x else y
-- condExpr :: Parser Expr
-- condExpr = do
--   reserved "if"
--   predicate <- expr
--   reserved "then"
--   thenBranch <- expr
--   reserved "else"
--   elseBranch <- expr
--   return $ Cond predicate thenBranch elseBranch

-- Records
-- Nested records are permitted
-- e.g. { a = 2, foo = true, c = 4 + 2, d = { bar = false } }
rcdExpr :: Parser Expr
rcdExpr = do
  reserved "{"
  fields <- sepBy rcdField (spaces *> char ',' <* spaces)
  reserved "}"
  return $ Rcd fields

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
           -- [binaryOp "*" Mul Exp.AssocLeft, binaryOp "/" Div Exp.AssocLeft],
           -- [binaryOp "+" Add Exp.AssocLeft, binaryOp "-" Sub Exp.AssocLeft],
           -- [binaryOp "<=" LTE Exp.AssocLeft, binaryOp ">=" GTE Exp.AssocLeft,
            -- binaryOp "<" LT Exp.AssocLeft, binaryOp ">" GT Exp.AssocLeft,
            -- binaryOp "==" Equal Exp.AssocLeft]]
           -- [typingOp "::" Exp.AssocLeft]]
           [projectionOp "." Exp.AssocLeft]]
  where
    projectionOp s = Exp.Infix $ reservedOp s >> return RcdProj
    -- typingOp s = Exp.Infix (reservedOp s >> return (flip TypedExpr) Nothing)
    -- binaryOp s op = Exp.Infix (reservedOp s >> return (BinOp op))
    -- Treat spaces as a binary operator for function application
    -- http://stackoverflow.com/questions/22904287/parsing-functional-application-with-parsec
    spacef = whiteSpace
      *> notFollowedBy (choice . map reservedOp $ opNames)
      >> return FApp

exprParsers = varExpr
          -- <|> letExpr
          <|> fnExpr
          -- <|> funExpr
          -- <|> condExpr
          <|> rcdExpr
          <|> intExpr
          -- <|> strExpr
          <|> parens expr

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""