module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

opNames :: [String]
opNames = ["+", "*", "-", "/", "==", ">=", "<="]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = emptyDef {
        Tok.commentLine = "#", 
        Tok.reservedOpNames = opNames,
        Tok.reservedNames = ["let", "in", "fn", "fun", "=", "if", "else", "then", "=>"] 
    }

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer
