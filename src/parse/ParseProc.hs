module ParseProc where

import qualified Ast
import qualified ParseStmt
import qualified Tokens

parseProc :: [Tokens.Token] -> (Ast.Procedure, [Tokens.Token])
parseProc (Tokens.Proc : Tokens.Ident name : tks) =
  (Ast.Proc name args stmts, tks3)
 where
  (args, tks2) = parseProcArgs tks
  (stmts, tks3) = parseProcBody tks2
parseProc tks = error ("parseProc error, tokens=" ++ show tks)

-- assumes first character is LBracket
-- consumes RBracket at end of token list
parseProcArgs :: [Tokens.Token] -> ([String], [Tokens.Token])
parseProcArgs (Tokens.LBracket : tokens) = ppah tokens []
 where
  ppah :: [Tokens.Token] -> [String] -> ([String], [Tokens.Token])
  ppah (Tokens.RBracket : tks) acc = (reverse acc, tks)
  ppah (Tokens.Ident name : toks@(Tokens.RBracket : _)) acc = ppah toks (name : acc)
  ppah (Tokens.Ident name : Tokens.Comma : tks) acc =
    case tks of
      (Tokens.Ident _ : _) -> ppah tks (name : acc)
      tks' -> error ("wrong comma placement" ++ show tks')
  ppah toks acc = error ("parseProcArgs error" ++ show toks ++ show acc)
parseProcArgs tokens = error ("expected LBracket, got=" ++ show (head tokens))

-- assumes first character is LBrace
-- consumes RBrace at end of token list
parseProcBody :: [Tokens.Token] -> ([Ast.Stmt], [Tokens.Token])
parseProcBody (Tokens.LBrace : tokens) = ppbh tokens []
 where
  ppbh :: [Tokens.Token] -> [Ast.Stmt] -> ([Ast.Stmt], [Tokens.Token])
  ppbh (Tokens.RBrace : tks) acc = (reverse acc, tks)
  ppbh tks acc =
    let (stmt, tks') = ParseStmt.parseStmt tks
     in ppbh tks' (stmt : acc)
parseProcBody tokens = error ("expected LBrace, got=" ++ show (head tokens))
