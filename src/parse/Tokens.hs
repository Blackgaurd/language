module Tokens where

data Token
  = Number String
  | Boolean Bool
  | Ident String
  | Add
  | Sub
  | Mult
  | Div
  | Lt -- less than
  | Gt -- greater than
  | Le -- less than or equal to
  | Ge -- greater than or equal to
  | Ee -- double equal
  | Ne -- not equal
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
