module Tokens where

data Token
  = Number String
  | Boolean Bool
  | Ident String
  | StringLit String
  | InfixIdent String
  | Add
  | Sub
  | Mult
  | Div
  | Mod
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
  | LBracket
  | RBracket
  | Comma
  | At
  | Semicolon
  | Proc
  | InfixL
  | InfixR
  | Return
  | When
  | Then
  | Otherwise
  | While
  | Break
  | Continue
  | Eof
  deriving (Show, Eq)
