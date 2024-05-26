module ParseStmt where

import qualified Ast
import qualified ParseExpr
import qualified Preprocess
import qualified Tokens

parseStmt :: Preprocess.ProcTypeMap -> [Tokens.Token] -> Bool -> (Ast.Stmt, [Tokens.Token])
parseStmt procMap (Tokens.Ident name : Tokens.Equal : tks) _ =
  let (expr, tks2) = ParseExpr.parseExpr procMap tks
   in case tks2 of
        (Tokens.Semicolon : tks3) -> (Ast.Set name expr, tks3)
        (tk : _) -> error ("expected semicolon, got=" ++ show tk)
        [] -> error "unexpected end of tokens"
parseStmt _ (Tokens.Return : Tokens.Semicolon : tks) _ = (Ast.Return Nothing, tks)
parseStmt procMap (Tokens.Return : tks) _ =
  let (expr, tks2) = ParseExpr.parseExpr procMap tks
   in case tks2 of
        (Tokens.Semicolon : tks3) -> (Ast.Return (Just expr), tks3)
        _ -> error ("expected semicolon, got=" ++ show (head tks2))
parseStmt procMap tokens@(Tokens.LBrace : _) isLoop =
  let (stmts, tks) = parseBlock procMap tokens isLoop
   in (Ast.BlockStmt stmts, tks)
parseStmt procMap tokens@(Tokens.When : _) isLoop = parseWhen procMap tokens isLoop
parseStmt procMap tokens@(Tokens.While : _) _ = parseWhile procMap tokens
parseStmt _ (Tokens.Break : Tokens.Semicolon : tks) isLoop =
  if isLoop
    then (Ast.Break, tks)
    else error "break statement not allowed outside of loop"
parseStmt _ (Tokens.Continue : Tokens.Semicolon : tks) isLoop =
  if isLoop
    then (Ast.Continue, tks)
    else error "continue statement not allowed outside of loop"
parseStmt procMap tokens _ =
  let (expr, tks) = ParseExpr.parseExpr procMap tokens
   in case tks of
        (Tokens.Semicolon : tks3) -> (Ast.Eval expr, tks3)
        _ -> error ("parseStmt: expected semicolon, got=" ++ show (head tks))

{- PARSE BLOCK -}
-- assumes first token is LBrace
-- consumes RBrace at end of list
parseBlock :: Preprocess.ProcTypeMap -> [Tokens.Token] -> Bool -> ([Ast.Stmt], [Tokens.Token])
parseBlock procMap (Tokens.LBrace : tokens) isLoop = helper tokens []
 where
  helper :: [Tokens.Token] -> [Ast.Stmt] -> ([Ast.Stmt], [Tokens.Token])
  helper (Tokens.RBrace : tks) acc = (reverse acc, tks)
  helper tks acc =
    let (stmt, tks') = parseStmt procMap tks isLoop
     in helper tks' (stmt : acc)
parseBlock _ tokens _ = error ("expected LBrace, got=" ++ show (head tokens))

stmtToBlock :: Ast.Stmt -> Ast.Block
stmtToBlock (Ast.BlockStmt stmts) = stmts
stmtToBlock stmt = [stmt]

{- PARSE WHEN STATEMENT -}
parseWhen :: Preprocess.ProcTypeMap -> [Tokens.Token] -> Bool -> (Ast.Stmt, [Tokens.Token])
parseWhen procMap (Tokens.When : tks) isLoop =
  let (expr, tks2) = ParseExpr.parseExpr procMap tks
   in case tks2 of
        (Tokens.Then : tks3) ->
          let (tStmt, tks4) = parseStmt procMap tks3 isLoop
           in case tks4 of
                (Tokens.Otherwise : tks5) ->
                  let (fStmt, tks6) = parseStmt procMap tks5 isLoop
                      tBlock = stmtToBlock tStmt
                      fBlock = stmtToBlock fStmt
                   in (Ast.WhenOtherwise expr tBlock fBlock, tks6)
                _ -> (Ast.When expr (stmtToBlock tStmt), tks4)
        _ -> error "parseWhen: did not get Then token as expected"
parseWhen _ tokens _ = error ("expected When, got=" ++ show (head tokens))

{- PARSE WHILE STATEMENT -}
parseWhile :: Preprocess.ProcTypeMap -> [Tokens.Token] -> (Ast.Stmt, [Tokens.Token])
parseWhile procMap (Tokens.While : tks) =
  let (expr, tks2) = ParseExpr.parseExpr procMap tks
   in case tks2 of
        (Tokens.Then : tks3) ->
          let (innerStmt, tks4) = parseStmt procMap tks3 True
              innerBlock = stmtToBlock innerStmt
           in (Ast.While expr innerBlock, tks4)
        _ -> error "parseWhile: did not get Then token as expected"
parseWhile _ tokens = error ("expected While, got=" ++ show (head tokens))
