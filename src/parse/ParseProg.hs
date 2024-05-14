module ParseProg where

import qualified Ast
import qualified ParseProc
import qualified Preprocess
import qualified Tokens

parseProg :: [Tokens.Token] -> Ast.Program
parseProg tokens =
  let procNames = Preprocess.getProcNames tokens
   in parseProgH procNames tokens []

parseProgH :: Preprocess.ProcTypeMap -> [Tokens.Token] -> [(Ast.Identifier, Ast.Procedure)] -> Ast.Program
parseProgH _ (Tokens.Eof : _) acc = Ast.Prog (reverse acc)
parseProgH procMap tokens acc =
  let (proc, tks) = ParseProc.parseProc procMap tokens
   in parseProgH procMap tks (proc : acc)
