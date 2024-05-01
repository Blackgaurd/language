module ParseProg where

import qualified Ast
import qualified ParseProc
import qualified Tokens

parseProg :: [Tokens.Token] -> Ast.Program
parseProg tokens = parseProgH tokens []

parseProgH :: [Tokens.Token] -> [(Ast.Identifier, Ast.Procedure)] -> Ast.Program
parseProgH (Tokens.Eof : _) acc = Ast.Prog (reverse acc)
parseProgH tokens acc = let (proc, tks) = ParseProc.parseProc tokens in parseProgH tks (proc : acc)
