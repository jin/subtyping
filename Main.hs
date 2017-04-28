module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment

import Parser (parseExpr)
import Typecheck

process :: String -> IO ()
process line = case parseExpr line of
  Left err -> print err
  Right ex -> do 
    putStrLn line
    print ex
    putStrLn ""

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
