module Main where

import Ast (Program)
import Lexer
import ParseProg
import TreeWalk (Value, interpProg)

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
  \ return j; \
  \}"

prog :: Program
prog = parseProg (tokenize code)

val :: Value
val = interpProg prog

main :: IO ()
main = print val
