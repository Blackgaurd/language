module Ast where

import Data.List (intercalate)

data UnOp = Pos | Neg deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | Mult
  | Div
  deriving (Show, Eq)

type Identifier = String

data Expr
  = Bin BinOp Expr Expr
  | Un UnOp Expr
  | Call Identifier [Expr]
  | Num String
  | Var Identifier
  deriving (Show, Eq)

data Stmt
  = Set Identifier Expr
  | Eval Expr
  | Return (Maybe Expr)
  deriving (Show, Eq)

type Body = [Stmt]

data Procedure = Proc Identifier [Identifier] Body deriving (Show, Eq)

newtype Program = Prog [Procedure] deriving (Show)

{- ----- PRINTING ----- -}
binOpToString :: BinOp -> String
binOpToString Add = "+"
binOpToString Sub = "-"
binOpToString Mult = "*"
binOpToString Div = "/"

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
exprToString (Var val) = val

stmtToString :: Stmt -> String
stmtToString (Set var value) = var ++ "=" ++ exprToString value ++ ";"
stmtToString (Eval expr) = exprToString expr ++ ";"
stmtToString (Return (Just expr)) = "return " ++ exprToString expr ++ ";"
stmtToString (Return Nothing) = "return;"

procToString :: Procedure -> String
procToString (Proc name args body) =
  "proc " ++ name ++ "(" ++ intercalate "," args ++ "){" ++ intercalate "" (map stmtToString body) ++ "}"
