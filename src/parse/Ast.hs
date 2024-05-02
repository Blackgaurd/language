module Ast where

import Data.List (intercalate)

data UnOp = Pos | Neg deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | Mult
  | Div
  | Lt -- less than
  | Gt -- greater than
  | Le -- less than or equal to
  | Ge -- greater than or equal to
  | Ee -- double equal
  | Ne -- not equal
  deriving (Show, Eq)

type Identifier = String

data Expr
  = Bin BinOp Expr Expr
  | Un UnOp Expr
  | Call Identifier [Expr]
  | Num String
  | Boolean Bool
  | Var Identifier
  deriving (Show, Eq)

data Stmt
  = Set Identifier Expr
  | Eval Expr
  | Return (Maybe Expr)
  deriving (Show, Eq)

type Body = [Stmt]

data Procedure = Proc [Identifier] Body deriving (Show, Eq)

newtype Program = Prog [(Identifier, Procedure)] deriving (Show)

{- ----- PRINTING ----- -}
binOpToString :: BinOp -> String
binOpToString Add = "+"
binOpToString Sub = "-"
binOpToString Mult = "*"
binOpToString Div = "/"
binOpToString Lt = "<"
binOpToString Gt = ">"
binOpToString Le = "<="
binOpToString Ge = ">="
binOpToString Ee = "=="
binOpToString Ne = "~="

unOpToString :: UnOp -> String
unOpToString Pos = "+"
unOpToString Neg = "-"

exprToString :: Expr -> String
exprToString (Bin op lhs rhs) =
  "(" ++ exprToString lhs ++ binOpToString op ++ exprToString rhs ++ ")"
exprToString (Un op rhs) = "(" ++ unOpToString op ++ exprToString rhs ++ ")"
exprToString (Call name args) = name ++ "(" ++ commaSep args ++ ")"
 where
  commaSep lst = intercalate "," (map exprToString lst)
exprToString (Num val) = val
exprToString (Boolean val) = if val then "!t" else "!f"
exprToString (Var val) = val

stmtToString :: Stmt -> String
stmtToString (Set var value) = var ++ "=" ++ exprToString value ++ ";"
stmtToString (Eval expr) = exprToString expr ++ ";"
stmtToString (Return (Just expr)) = "return " ++ exprToString expr ++ ";"
stmtToString (Return Nothing) = "return;"

procToString :: (Identifier, Procedure) -> String
procToString (name, Proc args body) =
  "proc " ++ name ++ "(" ++ intercalate "," args ++ "){" ++ intercalate "" (map stmtToString body) ++ "}"
