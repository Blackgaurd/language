module ParseProc where

import qualified Ast
import qualified LangUtils
import qualified ParseStmt
import qualified Preprocess
import qualified Tokens

-- TODO: parseInfix function

parseProc :: Preprocess.ProcTypeMap -> [Tokens.Token] -> ((Ast.Identifier, Ast.Procedure), [Tokens.Token])
parseProc procMap (Tokens.Proc : Tokens.Ident name : tks) =
  ((name, Ast.Proc args stmts), tks3)
 where
  (args, tks2) = parseProcArgs tks
  (stmts, tks3) = ParseStmt.parseBlock procMap tks2
parseProc procMap tokens =
  case tokens of
    (Tokens.InfixL : Tokens.Ident name : tks) ->
      let (prec, lhs, rhs, stmts, tks2) = parseInfixBody procMap tks
       in ((name, Ast.InfixL prec lhs rhs stmts), tks2)
    (Tokens.InfixR : Tokens.Ident name : tks) ->
      let (prec, lhs, rhs, stmts, tks2) = parseInfixBody procMap tks
       in ((name, Ast.InfixR prec lhs rhs stmts), tks2)
    _ -> error ("parseProc error, tokens=" ++ show tokens)

parseInfixBody :: Preprocess.ProcTypeMap -> [Tokens.Token] -> (Int, Ast.Identifier, Ast.Identifier, Ast.Block, [Tokens.Token])
parseInfixBody procMap tks = (prec, lhs, rhs, stmts, tks4)
 where
  (args, tks2) = parseProcArgs tks
  (lhs, rhs) = case args of
    [] -> error "infix operator must take 2 arguments, got=0"
    [_] -> error "infix operator must take 2 arguments, got=1"
    [l, r] -> (l, r)
    _ -> error ("infix operator must take 2 arguments, got=" ++ show (length args))
  (prec, tks3) = parseInfixPrec tks2
  (stmts, tks4) = ParseStmt.parseBlock procMap tks3

parseInfixPrec :: [Tokens.Token] -> (Int, [Tokens.Token])
parseInfixPrec (Tokens.LBracket : Tokens.Number prec : Tokens.RBracket : rest) =
  let intPrec = LangUtils.readBoundedInt prec in (intPrec, rest)
parseInfixPrec _ = error "parseInfixPrec: infix precedence declaration should take the form [prec]"

-- assumes first character is LParen
-- consumes RParen at end of token list
parseProcArgs :: [Tokens.Token] -> ([String], [Tokens.Token])
parseProcArgs (Tokens.LParen : tokens) = ppah tokens []
 where
  ppah :: [Tokens.Token] -> [String] -> ([String], [Tokens.Token])
  ppah (Tokens.RParen : tks) acc = (reverse acc, tks)
  ppah (Tokens.Ident name : toks@(Tokens.RParen : _)) acc = ppah toks (name : acc)
  ppah (Tokens.Ident name : Tokens.Comma : tks) acc =
    case tks of
      (Tokens.Ident _ : _) -> ppah tks (name : acc)
      tks' -> error ("wrong comma placement" ++ show tks')
  ppah toks acc = error ("parseProcArgs error" ++ show toks ++ show acc)
parseProcArgs tokens = error ("expected LParen, got=" ++ show (head tokens))
