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
parseStmt tokens@(Tokens.LBrace : _) = let (stmts, tks) = parseBlock tokens in (Ast.BlockStmt stmts, tks)
parseStmt tokens@(Tokens.When : _) = parseWhen tokens
parseStmt tokens@(Tokens.While : _) = parseWhile tokens
parseStmt tokens =
  let (expr, tks) = ParseExpr.parseExpr tokens
   in case tks of
        (Tokens.Semicolon : tks3) -> (Ast.Eval expr, tks3)
        _ -> error ("parseStmt: expected semicolon, got=" ++ show (head tks))

{- PARSE BLOCK -}
-- assumes first token is LBrace
-- consumes RBrace at end of list
parseBlock :: [Tokens.Token] -> ([Ast.Stmt], [Tokens.Token])
parseBlock (Tokens.LBrace : tokens) = helper tokens []
 where
  helper :: [Tokens.Token] -> [Ast.Stmt] -> ([Ast.Stmt], [Tokens.Token])
  helper (Tokens.RBrace : tks) acc = (reverse acc, tks)
  helper tks acc =
    let (stmt, tks') = ParseStmt.parseStmt tks
     in helper tks' (stmt : acc)
parseBlock tokens = error ("expected LBrace, got=" ++ show (head tokens))

stmtToBlock :: Ast.Stmt -> Ast.Block
stmtToBlock (Ast.BlockStmt stmts) = stmts
stmtToBlock stmt = [stmt]

{- PARSE WHEN STATEMENT -}
parseWhen :: [Tokens.Token] -> (Ast.Stmt, [Tokens.Token])
parseWhen (Tokens.When : tks) =
  let (expr, tks2) = ParseExpr.parseExpr tks
   in case tks2 of
        (Tokens.Then : tks3) ->
          let (tStmt, tks4) = parseStmt tks3
           in case tks4 of
                (Tokens.Otherwise : tks5) ->
                  let (fStmt, tks6) = parseStmt tks5
                   in (Ast.WhenOtherwise expr (stmtToBlock tStmt) (stmtToBlock fStmt), tks6)
                _ -> (Ast.When expr (stmtToBlock tStmt), tks4)
        _ -> error "parseWhen: did not get Then token as expected"
parseWhen tokens = error ("expected When, got=" ++ show (head tokens))

{- PARSE WHILE STATEMENT -}
parseWhile :: [Tokens.Token] -> (Ast.Stmt, [Tokens.Token])
parseWhile (Tokens.While : tks) =
  let (expr, tks2) = ParseExpr.parseExpr tks
   in case tks2 of
        (Tokens.Then : tks3) ->
          let (innerStmt, tks4) = parseStmt tks3
           in (Ast.While expr (stmtToBlock innerStmt), tks4)
        _ -> error "parseWhile: did not get Then token as expected"
parseWhile tokens = error ("expected While, got=" ++ show (head tokens))
