module ParseStmt where

import qualified Ast
import qualified ParseExpr
import qualified Tokens

parseStmt :: [Tokens.Token] -> (Ast.Stmt, [Tokens.Token])
parseStmt (Tokens.Ident name : Tokens.Equal : tks) =
  let (expr, tks2) = ParseExpr.parseExpr tks
   in case tks2 of
        (Tokens.Semicolon : tks3) -> (Ast.Set name expr, tks3)
        (tk : _) -> error ("expected semicolon, got=" ++ show tk)
        [] -> error "unexpected end of tokens"
parseStmt (Tokens.Return : Tokens.Semicolon : tks) = (Ast.Return Nothing, tks)
parseStmt (Tokens.Return : tks) =
  let (expr, tks2) = ParseExpr.parseExpr tks
   in case tks2 of
        (Tokens.Semicolon : tks3) -> (Ast.Return (Just expr), tks3)
        _ -> error ("expected semicolon, got=" ++ show (head tks2))
parseStmt tokens =
  let (expr, tks) = ParseExpr.parseExpr tokens
   in case tks of
        (Tokens.Semicolon : tks3) -> (Ast.Eval expr, tks3)
        _ -> error ("parseStmt: expected semicolon, got=" ++ show (head tks))
