module ParseExpr where

import qualified Ast
import qualified Data.Map as Map
import qualified LangUtils
import qualified Preprocess
import qualified Tokens

{-
PRECEDENCE RULES:
- higher precedence gets evaluated first
-}

{- ----- PARSE EXPRESSIONS ----- -}
minPrecedence :: Int
minPrecedence = 0

leftAssoc :: Int -> (Int, Int)
leftAssoc x = (2 * x, 2 * x + 1)

rightAssoc :: Int -> (Int, Int)
rightAssoc x = (2 * x + 1, 2 * x)

parseExpr :: Preprocess.ProcTypeMap -> [Tokens.Token] -> (Ast.Expr, [Tokens.Token])
parseExpr procMap tokens = parseExprPrec procMap tokens minPrecedence

-- assume next token is a number or ident
-- parseExprPrec :: tokens -> minPrec -> (expr, tokens)
parseExprPrec :: Preprocess.ProcTypeMap -> [Tokens.Token] -> Int -> (Ast.Expr, [Tokens.Token])
parseExprPrec procMap tokens@(Tokens.Add : _) minPrec =
  let (unexp, tks) = parsePrefix procMap tokens
   in parseInfix procMap unexp tks minPrec
parseExprPrec procMap tokens@(Tokens.Sub : _) minPrec =
  let (unexp, tks) = parsePrefix procMap tokens
   in parseInfix procMap unexp tks minPrec
parseExprPrec procMap tokens@(Tokens.LNot : _) minPrec =
  let (unexp, tks) = parsePrefix procMap tokens
   in parseInfix procMap unexp tks minPrec
parseExprPrec procMap (Tokens.LParen : tks) minPrec =
  let (lhs, tks2) = parseExprPrec procMap tks minPrecedence
   in parseInfix procMap lhs (tail tks2) minPrec
parseExprPrec procMap (Tokens.Number val : tks) minPrec =
  parseInfix procMap (Ast.Num val) tks minPrec
parseExprPrec procMap (Tokens.Boolean b : tks) minPrec =
  parseInfix procMap (Ast.Boolean b) tks minPrec
parseExprPrec procMap (Tokens.StringLit str : tks) minPrec =
  let strArray = LangUtils.stringToArray str
   in parseInfix procMap (Ast.StringLit strArray) tks minPrec
parseExprPrec procMap (Tokens.Ident name : tks) minPrec
  | head tks == Tokens.LParen =
      let (args, tks2) = parseExprList procMap tks
       in parseInfix procMap (Ast.Call name args) tks2 minPrec
  | otherwise = parseInfix procMap (Ast.Var name) tks minPrec
parseExprPrec _ tokens _ = error ("parseExprPrec error: " ++ show tokens)

{- ----- PREFIX OPERATOR ----- -}
toUnOp :: Tokens.Token -> Ast.UnOp
toUnOp Tokens.Add = Ast.Pos
toUnOp Tokens.Sub = Ast.Neg
toUnOp Tokens.LNot = Ast.LNot
toUnOp tk = error ("expected unary operator, got: " ++ show tk)

unOpPrec :: Ast.UnOp -> Int
unOpPrec Ast.Pos = 100
unOpPrec Ast.Neg = 100
unOpPrec Ast.LNot = 100

-- assume next token is unary operator
parsePrefix :: Preprocess.ProcTypeMap -> [Tokens.Token] -> (Ast.Expr, [Tokens.Token])
parsePrefix procMap (tk : tks) = (Ast.Un op rhs, tks2)
 where
  op = toUnOp tk
  rPrec = unOpPrec op
  (rhs, tks2) = parseExprPrec procMap tks rPrec
parsePrefix _ [] = error "can't parse prefix with no tokens"

{- ----- INFIX OPERATOR ----- -}
toBinOp :: Tokens.Token -> Ast.BinOp
toBinOp Tokens.Add = Ast.Add
toBinOp Tokens.Sub = Ast.Sub
toBinOp Tokens.Mult = Ast.Mult
toBinOp Tokens.Div = Ast.Div
toBinOp Tokens.Mod = Ast.Mod
toBinOp Tokens.Lt = Ast.Lt
toBinOp Tokens.Gt = Ast.Gt
toBinOp Tokens.Le = Ast.Le
toBinOp Tokens.Ge = Ast.Ge
toBinOp Tokens.Ee = Ast.Ee
toBinOp Tokens.Ne = Ast.Ne
toBinOp Tokens.LAnd = Ast.LAnd
toBinOp Tokens.LOr = Ast.LOr
toBinOp Tokens.At = Ast.At
toBinOp (Tokens.InfixIdent name) = Ast.InfixIdent name
toBinOp tk = error ("expected binary operator, got: " ++ show tk)

-- all builtin operators are left associative
binOpPrec :: Preprocess.ProcTypeMap -> Ast.BinOp -> (Int, Int)
binOpPrec procMap op =
  case op of
    Ast.InfixIdent name ->
      case Map.lookup name procMap of
        Nothing -> error ("infix ident not defined: " ++ name)
        Just Preprocess.Proc -> error ("can't use proc as infix, proc=" ++ name)
        Just (Preprocess.InfixL prec) -> leftAssoc prec
        Just (Preprocess.InfixR prec) -> rightAssoc prec
    Ast.LAnd -> leftAssoc 1
    Ast.LOr -> leftAssoc 1
    Ast.Ee -> leftAssoc 2
    Ast.Ne -> leftAssoc 2
    Ast.Lt -> leftAssoc 3
    Ast.Gt -> leftAssoc 3
    Ast.Le -> leftAssoc 3
    Ast.Ge -> leftAssoc 3
    Ast.Add -> leftAssoc 4
    Ast.Sub -> leftAssoc 4
    Ast.Mult -> leftAssoc 5
    Ast.Div -> leftAssoc 5
    Ast.Mod -> leftAssoc 5
    Ast.At -> leftAssoc 6

-- assume next char is infix operator
-- parseInfix :: left -> tokens -> minPrec -> (expr, tokens)
parseInfix :: Preprocess.ProcTypeMap -> Ast.Expr -> [Tokens.Token] -> Int -> (Ast.Expr, [Tokens.Token])
parseInfix _ lhs tokens@(Tokens.Eof : _) _ = (lhs, tokens) -- should be error, unexpected eof
parseInfix _ lhs tokens@(Tokens.Semicolon : _) _ = (lhs, tokens)
parseInfix _ lhs tokens@(Tokens.Comma : _) _ = (lhs, tokens) -- TODO: only allow this when parsing expression list
parseInfix _ lhs tokens@(Tokens.Equal : _) _ = (lhs, tokens) -- TODO: should be error, implement later
parseInfix _ lhs tokens@(Tokens.RParen : _) _ = (lhs, tokens)
parseInfix _ lhs tokens@(Tokens.Then : _) _ = (lhs, tokens)
parseInfix procMap lhs tokens@(tk : tks) minPrec
  | minPrec < lPrec =
      let (rhs, tks2) = parseExprPrec procMap tks rPrec
       in parseInfix procMap (Ast.Bin op lhs rhs) tks2 minPrec
  | otherwise = (lhs, tokens)
 where
  op = toBinOp tk
  (lPrec, rPrec) = binOpPrec procMap op
parseInfix _ lhs tks minPrec =
  error
    ( "parseInfix error, lhs: "
        ++ show lhs
        ++ ", tks: "
        ++ show tks
        ++ ", minPrec: "
        ++ show minPrec
    )

{- ----- PARSE EXPR LIST ----- -}
-- assumes first token is LParen,
-- consumes RParen at end of list
parseExprList :: Preprocess.ProcTypeMap -> [Tokens.Token] -> ([Ast.Expr], [Tokens.Token])
parseExprList procMap (Tokens.LParen : tks) = parseExprListH procMap tks []
parseExprList _ (tk : _) = error ("expected lbracket, got=" ++ show tk)
parseExprList _ [] = error "parseExprList: unexpected end of tokens"

parseExprListH :: Preprocess.ProcTypeMap -> [Tokens.Token] -> [Ast.Expr] -> ([Ast.Expr], [Tokens.Token])
parseExprListH _ (Tokens.RParen : tks) acc = (reverse acc, tks)
parseExprListH procMap tks acc =
  let (expr, tks2) = parseExprPrec procMap tks minPrecedence
   in case tks2 of
        (Tokens.Comma : tks3) -> parseExprListH procMap tks3 (expr : acc)
        (Tokens.RParen : _) -> parseExprListH procMap tks2 (expr : acc)
        (tk : _) -> error ("parseExprListH: expected comma or rbracket, got=" ++ show tk)
        [] -> error "parseExprListH: unexpected end of tokens"
