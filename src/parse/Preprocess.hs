module Preprocess where

import qualified Ast
import qualified Data.Map as Map
import qualified LangUtils
import qualified Tokens

data ProcType
  = Proc
  | InfixL Int
  | InfixR Int

type ProcTypeMap = Map.Map Ast.Identifier ProcType

getProcNames :: [Tokens.Token] -> ProcTypeMap
getProcNames = getProcNamesH []

getProcNamesH :: [(Ast.Identifier, ProcType)] -> [Tokens.Token] -> ProcTypeMap
getProcNamesH acc (Tokens.Eof : _) = Map.fromList acc
getProcNamesH acc (procKeyword : (Tokens.Ident name) : rest) =
  case procKeyword of
    Tokens.Proc ->
      let tks = consumeBracketPair rest -- ( to )
          tks2 = consumeBracketPair tks -- { to }
       in getProcNamesH ((name, Proc) : acc) tks2
    Tokens.InfixL ->
      let tks = consumeBracketPair rest -- ( to )
          (prec, tks2) = getInfixPrec tks -- [ to ]
          tks3 = consumeBracketPair tks2 -- { to }
       in getProcNamesH ((name, InfixL prec) : acc) tks3
    Tokens.InfixR ->
      let tks = consumeBracketPair rest -- ( to )
          (prec, tks2) = getInfixPrec tks -- [ to ]
          tks3 = consumeBracketPair tks2 -- { to }
       in getProcNamesH ((name, InfixR prec) : acc) tks3
    x -> error ("getProcNames: expected 'proc', 'infixl' or 'infixr', got=" ++ show x)
getProcNamesH _ tks = error ("getProcNames: unexpected sequence of tokens, got=" ++ show tks)

getInfixPrec :: [Tokens.Token] -> (Int, [Tokens.Token])
getInfixPrec (Tokens.LBracket : Tokens.Number prec : Tokens.RBracket : rest) =
  let intPrec = LangUtils.readBoundedInt prec in (intPrec, rest)
getInfixPrec _ = error "getInfixPrec: infix precedence declaration should take the form [prec]"

consumeBracketPair :: [Tokens.Token] -> [Tokens.Token]
consumeBracketPair [] = error "consumeBracketPair: unexpected end of tokens"
consumeBracketPair (open : rest) =
  case open of
    Tokens.LParen -> helper rest [open]
    Tokens.LBracket -> helper rest [open]
    Tokens.LBrace -> helper rest [open]
    _ -> error "expected open paren-like"
 where
  helper :: [Tokens.Token] -> [Tokens.Token] -> [Tokens.Token]
  helper tks [] = tks
  helper tks stk =
    case LangUtils.safeHead tks of
      Nothing -> error "unexpected end of tokens"
      Just Tokens.LParen -> helper (tail tks) (Tokens.LParen : stk)
      Just Tokens.LBracket -> helper (tail tks) (Tokens.LBracket : stk)
      Just Tokens.LBrace -> helper (tail tks) (Tokens.LBrace : stk)
      Just Tokens.RParen ->
        case LangUtils.safeHead stk of
          Nothing -> error "unexpected end of tokens"
          Just Tokens.LParen -> helper (tail tks) (tail stk)
          Just x -> error ("expected LParen, got=" ++ show x)
      Just Tokens.RBracket ->
        case LangUtils.safeHead stk of
          Nothing -> error "unexpected end of tokens"
          Just Tokens.LBracket -> helper (tail tks) (tail stk)
          Just x -> error ("expected LBracket, got=" ++ show x)
      Just Tokens.RBrace ->
        case LangUtils.safeHead stk of
          Nothing -> error "unexpected end of tokens"
          Just Tokens.LBrace -> helper (tail tks) (tail stk)
          Just x -> error ("expected LBrace, got=" ++ show x)
      Just _ -> helper (tail tks) stk
