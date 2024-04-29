module Lexer where

import Data.Char (isAlpha, isDigit)
import GHC.Unicode (isSpace)
import Tokens

-- assumes first character is a digit
-- readNumber :: input -> (Number, rest of input)
readNumber :: String -> (Token, String)
readNumber s = rnh s []
 where
  rnh :: String -> String -> (Token, String)
  rnh [] acc = (Number (reverse acc), [])
  rnh str@(ch : rest) acc
    | isDigit ch = rnh rest (ch : acc)
    | otherwise = (Number (reverse acc), str)

matchKeyword :: String -> Token
matchKeyword "proc" = Proc
matchKeyword "return" = Return
matchKeyword str = Ident str

-- assumes first character is alpha
-- readIdent :: input -> (Ident, rest of input)
readIdent :: String -> (Token, String)
readIdent s =
  let (str, rest) = rih s []
   in (matchKeyword str, rest)
 where
  rih :: String -> String -> (String, String)
  rih [] acc = (reverse acc, [])
  rih str@(ch : rest) acc
    | isAlpha ch = rih rest (ch : acc)
    | ch == '_' = rih rest (ch : acc)
    | otherwise = (reverse acc, str)

-- _tokenize :: input -> acc -> Tokens
_tokenize :: String -> [Token] -> [Token]
_tokenize [] acc = reverse (Eof : acc)
_tokenize str@(ch : chs) acc
  | isSpace ch = _tokenize chs acc
  | isDigit ch = let (num, rest) = readNumber str in _tokenize rest (num : acc)
  | isAlpha ch || ch == '_' = let (ident, rest) = readIdent str in _tokenize rest (ident : acc)
  | ch == '+' = _tokenize chs (Add : acc)
  | ch == '-' = _tokenize chs (Sub : acc)
  | ch == '*' = _tokenize chs (Mult : acc)
  | ch == '/' = _tokenize chs (Div : acc)
  | ch == '=' = _tokenize chs (Equal : acc)
  | ch == '(' = _tokenize chs (LBracket : acc)
  | ch == ')' = _tokenize chs (RBracket : acc)
  | ch == '{' = _tokenize chs (LBrace : acc)
  | ch == '}' = _tokenize chs (RBrace : acc)
  | ch == ';' = _tokenize chs (Semicolon : acc)
  | ch == ',' = _tokenize chs (Comma : acc)
  | otherwise = error ("unrecognized character: " ++ [ch])

-- tokenize :: input -> Tokens
tokenize :: String -> [Token]
tokenize s = _tokenize s []

-- checkBrackets :: Tokens -> whether brackets match
checkBrackets :: [Token] -> Bool
checkBrackets tokens = cbh tokens []
 where
  cbh :: [Token] -> [Token] -> Bool
  cbh [] [] = True
  cbh [] _ = False
  cbh (LBracket : tks) stk = cbh tks (LBracket : stk)
  cbh (RBracket : tks) (LBracket : stk) = cbh tks stk
  cbh (RBracket : _) _ = False
  cbh (LBrace : tks) stk = cbh tks (LBrace : stk)
  cbh (RBrace : tks) (LBrace : stk) = cbh tks stk
  cbh (RBrace : _) _ = False
  cbh (_ : tks) stk = cbh tks stk
