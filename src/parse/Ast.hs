module Ast where

import qualified Data.Array as Array
import Data.List (intercalate)

type Identifier = String

data UnOp = Pos | Neg | LNot deriving (Show, Eq)

data BinOp
  = Add
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
  | LOr -- logical or
  | LAnd -- logical and
  | At
  | InfixIdent Identifier
  deriving (Show, Eq)

data Expr
  = Bin BinOp Expr Expr
  | Un UnOp Expr
  | Call Identifier [Expr]
  | Num String
  | Boolean Bool
  | Var Identifier
  | StringLit (Array.Array Integer Char)
  deriving (Show, Eq)

data Stmt
  = Set Identifier Expr
  | Eval Expr
  | Return (Maybe Expr)
  | BlockStmt Block
  | When Expr Block
  | WhenOtherwise Expr Block Block
  | While Expr Block
  deriving (Show, Eq)

type Block = [Stmt]

data Procedure
  = Proc [Identifier] Block
  | InfixL Int Identifier Identifier Block
  | InfixR Int Identifier Identifier Block
  deriving (Show, Eq)

newtype Program = Prog [(Identifier, Procedure)] deriving (Show)

{- ----- PRINTING ----- -}
binOpToString :: BinOp -> String
binOpToString Add = "+"
binOpToString Sub = "-"
binOpToString Mult = "*"
binOpToString Div = "/"
binOpToString Mod = "%"
binOpToString Lt = "<"
binOpToString Gt = ">"
binOpToString Le = "<="
binOpToString Ge = ">="
binOpToString Ee = "=="
binOpToString Ne = "~="
binOpToString LOr = "|"
binOpToString LAnd = "&"
binOpToString At = "@"
binOpToString (InfixIdent name) = "`" ++ name ++ "`"

unOpToString :: UnOp -> String
unOpToString Pos = "+"
unOpToString Neg = "-"
unOpToString LNot = "~"

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
exprToString (StringLit str) = "\"" ++ Array.elems str ++ "\""

stmtToString :: Stmt -> String
stmtToString (Set var value) = var ++ "=" ++ exprToString value ++ ";"
stmtToString (Eval expr) = exprToString expr ++ ";"
stmtToString (Return (Just expr)) = "return " ++ exprToString expr ++ ";"
stmtToString (Return Nothing) = "return;"
stmtToString (BlockStmt stmts) = "{" ++ intercalate "" (map stmtToString stmts) ++ "}"
stmtToString (When expr stmts) = "when(" ++ exprToString expr ++ ")then{" ++ intercalate "" (map stmtToString stmts) ++ "}"
stmtToString (WhenOtherwise expr tStmts fStmts) =
  let tString = intercalate "" (map stmtToString tStmts)
      fString = intercalate "" (map stmtToString fStmts)
   in "when(" ++ exprToString expr ++ ")then{" ++ tString ++ "}otherwise{" ++ fString ++ "}"
stmtToString (While expr stmts) = "while(" ++ exprToString expr ++ ")then{" ++ intercalate "" (map stmtToString stmts) ++ "}"

procToString :: (Identifier, Procedure) -> String
procToString (name, Proc args body) =
  "proc " ++ name ++ "(" ++ intercalate "," args ++ "){" ++ intercalate "" (map stmtToString body) ++ "}"
procToString (name, InfixL prec lhs rhs body) =
  "infixl " ++ name ++ "(" ++ lhs ++ "," ++ rhs ++ ")[" ++ show prec ++ "]{" ++ intercalate "" (map stmtToString body) ++ "}"
procToString (name, InfixR prec lhs rhs body) =
  "infixr " ++ name ++ "(" ++ lhs ++ "," ++ rhs ++ ")[" ++ show prec ++ "]{" ++ intercalate "" (map stmtToString body) ++ "}"

-- newtype Program = Prog [(Identifier, Procedure)] deriving (Show)
progToString :: Program -> String
progToString (Prog procs) = intercalate "" (map procToString procs)
