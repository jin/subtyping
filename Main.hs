module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment

import Parser (parseExpr)
import Typecheck (typecheckExpr)

ansiGreen :: String 
ansiGreen = "\x1b[32m"

ansiRed :: String 
ansiRed = "\x1b[31m" 

ansiReset :: String 
ansiReset = "\x1b[0m"

process :: String -> IO ()
process line = case parseExpr line of
  Left err -> print err
  Right ex -> do 
    putStrLn $ "[Expression]: " ++ line
    putStrLn $ "[Typecheck] " ++ res
    putStrLn $ "[AST]: " ++ show ex
    putStrLn ""
      where res = case typecheckExpr ex of 
                       Left err -> "[" ++ ansiRed ++ "FAIL" ++ ansiReset ++ "]: " ++ err
                       Right ty -> "[" ++ ansiGreen ++ "OK" ++ ansiReset ++ "]: " ++ show ty

-- https://hackage.haskell.org/package/haskeline-0.7.3.1/docs/System-Console-Haskeline.html
runRepl :: IO ()
runRepl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "> "
      case minput of
           Nothing -> outputStrLn "Goodbye."
           Just input -> liftIO (process input) >> loop

loadFile :: String -> IO ()
loadFile filePath = do
  src <- readFile filePath
  mapM_ process [line | line <- lines src, line /= ""]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    (x:_) -> loadFile x
