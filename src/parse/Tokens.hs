module Tokens where

data Token
  = Number String
  | Ident String
  | Add
  | Sub
  | Mult
  | Div
  | Equal
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Semicolon
  | Proc
  | Return
  | Eof
  deriving (Show, Eq)
