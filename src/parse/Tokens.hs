module Tokens where

data Token
  = Number String
  | Boolean Bool
  | Ident String
  | StringLit String
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
  | LNot -- logical not
  | LOr -- logical or
  | LAnd -- logical and
  | Equal
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Comma
  | At
  | Semicolon
  | Proc
  | Return
  | When
  | Then
  | Otherwise
  | Eof
  deriving (Show, Eq)
