module ParseExpr where

import qualified Ast
import qualified Tokens

{-
PRECEDENCE RULES:
- higher precedence gets evaluated first
-}

{- ----- PARSE EXPRESSIONS ----- -}
minPrecedence :: Int
minPrecedence = 0

parseExpr :: [Tokens.Token] -> (Ast.Expr, [Tokens.Token])
parseExpr tokens = parseExprPrec tokens minPrecedence

-- assume next token is a number or ident
-- parseExprPrec :: tokens -> minPrec -> (expr, tokens)
parseExprPrec :: [Tokens.Token] -> Int -> (Ast.Expr, [Tokens.Token])
parseExprPrec tokens@(Tokens.Add : _) minPrec =
  let (unexp, tks) = parsePrefix tokens
   in parseInfix unexp tks minPrec
parseExprPrec tokens@(Tokens.Sub : _) minPrec =
  let (unexp, tks) = parsePrefix tokens
   in parseInfix unexp tks minPrec
parseExprPrec (Tokens.LBracket : tks) minPrec =
  let (lhs, tks2) = parseExprPrec tks minPrecedence
   in parseInfix lhs (tail tks2) minPrec
parseExprPrec (Tokens.Number val : tks) minPrec = parseInfix (Ast.Num val) tks minPrec
parseExprPrec (Tokens.Boolean b : tks) minPrec = parseInfix (Ast.Boolean b) tks minPrec
parseExprPrec (Tokens.Ident name : tks) minPrec
  | head tks == Tokens.LBracket =
      let (args, tks2) = parseExprList tks
       in parseInfix (Ast.Call name args) tks2 minPrec
  | otherwise = parseInfix (Ast.Var name) tks minPrec
parseExprPrec tokens minPrec = error ("parseExprPrec error" ++ show tokens)

{- ----- PREFIX OPERATOR ----- -}
toUnOp :: Tokens.Token -> Ast.UnOp
toUnOp Tokens.Add = Ast.Pos
toUnOp Tokens.Sub = Ast.Neg
toUnOp tk = error ("expected unary operator, got: " ++ show tk)

unOpPrec :: Ast.UnOp -> Int
unOpPrec Ast.Pos = 7
unOpPrec Ast.Neg = 7

-- assume next token is unary operator
parsePrefix :: [Tokens.Token] -> (Ast.Expr, [Tokens.Token])
parsePrefix (tk : tks) = (Ast.Un op rhs, tks2)
 where
  op = toUnOp tk
  rPrec = unOpPrec op
  (rhs, tks2) = parseExprPrec tks rPrec
parsePrefix [] = error "can't parse prefix with no tokens"

{- ----- INFIX OPERATOR ----- -}
toBinOp :: Tokens.Token -> Ast.BinOp
toBinOp Tokens.Add = Ast.Add
toBinOp Tokens.Sub = Ast.Sub
toBinOp Tokens.Mult = Ast.Mult
toBinOp Tokens.Div = Ast.Div
toBinOp Tokens.Lt = Ast.Lt
toBinOp Tokens.Gt = Ast.Gt
toBinOp Tokens.Le = Ast.Le
toBinOp Tokens.Ge = Ast.Ge
toBinOp Tokens.Ee = Ast.Ee
toBinOp Tokens.Ne = Ast.Ne
toBinOp tk = error ("expected binary operator, got: " ++ show tk)

binOpPrec :: Ast.BinOp -> (Int, Int)
binOpPrec Ast.Lt = (1, 2)
binOpPrec Ast.Gt = (1, 2)
binOpPrec Ast.Le = (1, 2)
binOpPrec Ast.Ge = (1, 2)
binOpPrec Ast.Ee = (1, 2)
binOpPrec Ast.Ne = (1, 2)
binOpPrec Ast.Add = (3, 4)
binOpPrec Ast.Sub = (3, 4)
binOpPrec Ast.Mult = (5, 6)
binOpPrec Ast.Div = (5, 6)

-- assume next char is infix operator
-- parseInfix :: left -> tokens -> minPrec -> (expr, tokens)
parseInfix :: Ast.Expr -> [Tokens.Token] -> Int -> (Ast.Expr, [Tokens.Token])
parseInfix lhs tokens@(Tokens.Eof : _) _ = (lhs, tokens) -- should be error, unexpected eof
parseInfix lhs tokens@(Tokens.Semicolon : _) _ = (lhs, tokens)
parseInfix lhs tokens@(Tokens.Comma : _) _ = (lhs, tokens) -- TODO: only allow this when parsing expression list
parseInfix lhs tokens@(Tokens.Equal : _) _ = (lhs, tokens) -- TODO: should be error, implement later
parseInfix lhs tokens@(Tokens.RBracket : _) _ = (lhs, tokens)
parseInfix lhs tokens@(tk : tks) minPrec
  | minPrec < lPrec =
      let (rhs, tks') = parseExprPrec tks rPrec
       in parseInfix (Ast.Bin op lhs rhs) tks' minPrec
  | otherwise = (lhs, tokens)
 where
  op = toBinOp tk
  (lPrec, rPrec) = binOpPrec op
parseInfix lhs tks minPrec =
  error
    ( "parseInfix error, lhs: "
        ++ show lhs
        ++ ", tks: "
        ++ show tks
        ++ ", minPrec: "
        ++ show minPrec
    )

{- ----- PARSE EXPR LIST ----- -}
-- assumes first token is LBracket,
-- consumes RBracket at end of list
parseExprList :: [Tokens.Token] -> ([Ast.Expr], [Tokens.Token])
parseExprList (Tokens.LBracket : tks) = parseExprListH tks []
parseExprList (tk : _) = error ("expected lbracket, got=" ++ show tk)
parseExprList [] = error "parseExprList: unexpected end of tokens"

parseExprListH :: [Tokens.Token] -> [Ast.Expr] -> ([Ast.Expr], [Tokens.Token])
parseExprListH (Tokens.RBracket : tks) acc = (reverse acc, tks)
parseExprListH tks acc =
  let (expr, tks2) = parseExprPrec tks minPrecedence
   in case tks2 of
        (Tokens.Comma : tks3) -> parseExprListH tks3 (expr : acc)
        (Tokens.RBracket : _) -> parseExprListH tks2 (expr : acc)
        (tk : _) -> error ("parseExprListH: expected comma or rbracket, got=" ++ show tk)
        [] -> error "parseExprListH: unexpected end of tokens"
