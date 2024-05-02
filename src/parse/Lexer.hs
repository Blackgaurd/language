module Lexer where

import Data.Char (isAlpha, isDigit)
import GHC.Unicode (isSpace)
import Tokens
import qualified Utils

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlpha ch || ch == '_' || isDigit ch || ch == '!' || ch == '?'

readWord :: String -> (String, String)
readWord s = helper s []
 where
  helper :: String -> String -> (String, String)
  helper [] acc = (reverse acc, [])
  helper str@(ch : rest) acc
    | isIdentifierChar ch = helper rest (ch : acc)
    | otherwise = (reverse acc, str)

matchKeyword :: String -> Token
matchKeyword "proc" = Proc
matchKeyword "return" = Return
matchKeyword "!t" = Boolean True
matchKeyword "!f" = Boolean False
matchKeyword str = Ident str

matchChar :: String -> (Token, String)
matchChar [] = error "unexpected eof"
matchChar (ch : chs) =
  case ch of
    '+' -> (Add, chs)
    '-' -> (Sub, chs)
    '*' -> (Mult, chs)
    '/' -> (Div, chs)
    '(' -> (LBracket, chs)
    ')' -> (RBracket, chs)
    '{' -> (LBrace, chs)
    '}' -> (RBrace, chs)
    ';' -> (Semicolon, chs)
    ',' -> (Comma, chs)
    '=' -> case Utils.safeHead chs of
      Nothing -> (Equal, chs)
      Just '=' -> (Ee, tail chs)
      Just _ -> (Equal, chs)
    '>' -> case Utils.safeHead chs of
      Nothing -> (Gt, chs)
      Just '=' -> (Ge, tail chs)
      Just _ -> (Gt, chs)
    '<' -> case Utils.safeHead chs of
      Nothing -> (Lt, chs)
      Just '=' -> (Le, tail chs)
      Just _ -> (Lt, chs)
    '~' -> case Utils.safeHead chs of
      Nothing -> error "expected character after ~"
      Just '=' -> (Ne, tail chs)
      Just _ -> error "unrecognized character after ~"
    _ -> error ("unrecognized character: " ++ [ch])

-- tokenizeH :: input -> acc -> Tokens
tokenizeH :: String -> [Token] -> [Token]
tokenizeH [] acc = reverse (Eof : acc)
tokenizeH str@(ch : chs) acc
  | isSpace ch = tokenizeH chs acc
  | isIdentifierChar ch =
      let (word, rest) = readWord str
          token = if all isDigit word then Number word else matchKeyword word
       in tokenizeH rest (token : acc)
  | otherwise = let (token, rest) = matchChar str in tokenizeH rest (token : acc)

-- tokenize :: input -> Tokens
tokenize :: String -> [Token]
tokenize s = tokenizeH s []

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
