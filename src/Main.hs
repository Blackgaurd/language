module Main where

import Ast (Program)
import Lexer
import ParseProg
import TreeWalk (interpProg)

code :: String
code =
  "proc doNothing(x, y) {\
  \ x = x + 1; \
  \ y = y + 1; \
  \ z = x + y; \
  \ return z + z + z * 4; \
  \} \
  \\
  \proc main() { \
  \ j = doNothing(1, 2); \
  \ disp(1); \
  \ return j; \
  \}"

prog :: Program
prog = parseProg (tokenize code)

main :: IO ()
main = do
  returnValue <- interpProg prog
  print returnValue
