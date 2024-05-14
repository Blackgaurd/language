module Lexer where

import Data.Char (isAlpha, isDigit)
import GHC.Unicode (isSpace)
import qualified LangUtils
import Tokens

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
matchKeyword "when" = When
matchKeyword "then" = Then
matchKeyword "otherwise" = Otherwise
matchKeyword str = Ident str

nextToken :: String -> (Token, String)
nextToken [] = (Eof, [])
nextToken str@(ch : chs)
  | isSpace ch = nextToken chs
  | isIdentifierChar ch =
      let (word, rest) = readWord str
          token = if all isDigit word then Number word else matchKeyword word
       in (token, rest)
  | otherwise = case ch of
      '+' -> (Add, chs)
      '-' -> (Sub, chs)
      '*' -> (Mult, chs)
      '/' -> (Div, chs)
      '%' -> case LangUtils.safeHead chs of
        Nothing -> (Mod, chs)
        Just '%' ->
          let rest = consumeUntil (\c -> c == '\n' || c == '\r') (tail chs)
           in nextToken rest
        Just _ -> (Mod, chs)
      '(' -> (LParen, chs)
      ')' -> (RParen, chs)
      '{' -> (LBrace, chs)
      '}' -> (RBrace, chs)
      '@' -> (At, chs)
      ';' -> (Semicolon, chs)
      ',' -> (Comma, chs)
      '&' -> (LAnd, chs)
      '|' -> (LOr, chs)
      '"' -> readStringLit str
      '=' -> case LangUtils.safeHead chs of
        Nothing -> (Equal, chs)
        Just '=' -> (Ee, tail chs)
        Just _ -> (Equal, chs)
      '>' -> case LangUtils.safeHead chs of
        Nothing -> (Gt, chs)
        Just '=' -> (Ge, tail chs)
        Just _ -> (Gt, chs)
      '<' -> case LangUtils.safeHead chs of
        Nothing -> (Lt, chs)
        Just '=' -> (Le, tail chs)
        Just _ -> (Lt, chs)
      '~' -> case LangUtils.safeHead chs of
        Nothing -> error "expected character after ~"
        Just '=' -> (Ne, tail chs)
        Just _ -> (LNot, chs)
      _ -> error ("unrecognized character: " ++ [ch])

readStringLit :: String -> (Token, String)
readStringLit ('"' : chrs) = helper chrs []
 where
  helper :: String -> String -> (Token, String)
  helper [] _ = error "unexpected eof when reading string"
  helper ('"' : rest) acc = (StringLit (reverse acc), rest)
  helper ('\\' : rest) acc =
    case LangUtils.safeHead rest of
      Nothing -> error "unclosed escape character"
      Just c ->
        let escaped = matchEscapeCharacter c
         in helper (tail rest) (escaped : acc)
  helper (ch : rest) acc = helper rest (ch : acc)
readStringLit (x : _) = error ("expected quote to read string, got=" ++ [x])
readStringLit [] = error "unexpected eof when reading string"

matchEscapeCharacter :: Char -> Char
matchEscapeCharacter ch =
  case ch of
    'n' -> '\n'
    '\\' -> '\\'
    '"' -> '"'
    _ -> error ("unrecognized escape character: " ++ [ch])

consumeUntil :: (Char -> Bool) -> String -> String
consumeUntil _ [] = []
consumeUntil check (ch : chs)
  | check ch = chs
  | otherwise = consumeUntil check chs

-- tokenizeH :: input -> acc -> Tokens
tokenizeH :: String -> [Token] -> [Token]
tokenizeH [] acc =
  case LangUtils.safeHead acc of
    Nothing -> [Eof]
    Just Eof -> reverse acc
    Just _ -> reverse (Eof : acc)
tokenizeH str acc = let (tok, rest) = nextToken str in tokenizeH rest (tok : acc)

-- tokenize :: input -> Tokens
tokenize :: String -> [Token]
tokenize s = tokenizeH s []

-- checkParens :: Tokens -> whether brackets match
checkParens :: [Token] -> Bool
checkParens tokens = cbh tokens []
 where
  cbh :: [Token] -> [Token] -> Bool
  cbh [] [] = True
  cbh [] _ = False
  cbh (LParen : tks) stk = cbh tks (LParen : stk)
  cbh (RParen : tks) (LParen : stk) = cbh tks stk
  cbh (RParen : _) _ = False
  cbh (LBrace : tks) stk = cbh tks (LBrace : stk)
  cbh (RBrace : tks) (LBrace : stk) = cbh tks stk
  cbh (RBrace : _) _ = False
  cbh (_ : tks) stk = cbh tks stk
