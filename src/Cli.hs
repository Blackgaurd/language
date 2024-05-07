module Cli where

import qualified Ast
import Data.List (intercalate)
import qualified Lexer
import qualified ParseProg
import qualified TreeWalk

interpFile :: String -> IO ()
interpFile filename = do
  contents <- readFile filename
  returnValue <- TreeWalk.interpString contents
  putStrLn ("exit code: " ++ show returnValue)

minimizeFile :: String -> IO String
minimizeFile filename = do
  contents <- readFile filename
  let (Ast.Prog procs) = ParseProg.parseProg (Lexer.tokenize contents)
   in return (intercalate "" (map Ast.procToString procs))
