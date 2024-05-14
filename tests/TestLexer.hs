module Main where

import Lexer
import Test.HUnit
import Tokens

tokenizeTests =
  [ ("1", [Number "1", Eof])
  , ("a", [Ident "a", Eof])
  , ("_123", [Ident "_123", Eof])
  , ("Aa77aa__", [Ident "Aa77aa__", Eof])
  , ("987654321_", [Ident "987654321_", Eof])
  , ("!!", [Ident "!!", Eof])
  , ("?!_", [Ident "?!_", Eof])
  , ("!t", [Boolean True, Eof])
  , ("!f", [Boolean False, Eof])
  , ("~~!t", [LNot, LNot, Boolean True, Eof])
  , ("~&!t", [LNot, LAnd, Boolean True, Eof])
  , ("|~!t", [LOr, LNot, Boolean True, Eof])
  , ("   1234  ", [Number "1234", Eof])
  , (" \"string\" ", [StringLit "string", Eof])
  , (" \"string1\" + \"string2\" ", [StringLit "string1", Add, StringLit "string2", Eof])
  , ("AbFwdd adja", [Ident "AbFwdd", Ident "adja", Eof])
  , ("\"apples\" @ 14", [StringLit "apples", At, Number "14", Eof])
  , ("1 + 2 * four - +orange  / 9", [Number "1", Add, Number "2", Mult, Ident "four", Sub, Add, Ident "orange", Div, Number "9", Eof])
  , ("7 % 14", [Number "7", Mod, Number "14", Eof])
  , ("when 1 + 2 == 3 then a = 1; otherwise {a = 2;}", [When, Number "1", Add, Number "2", Ee, Number "3", Then, Ident "a", Equal, Number "1", Semicolon, Otherwise, LBrace, Ident "a", Equal, Number "2", Semicolon, RBrace, Eof])
  , ("while i == j then {i = i + j;}", [While, Ident "i", Ee, Ident "j", Then, LBrace, Ident "i", Equal, Ident "i", Add, Ident "j", Semicolon, RBrace, Eof])
  , ("(((1)))", [LParen, LParen, LParen, Number "1", RParen, RParen, RParen, Eof])
  , ("(+1)-(+  2)", [LParen, Add, Number "1", RParen, Sub, LParen, Add, Number "2", RParen, Eof])
  , ("1 <= 2 < 3 > 4 >= 5 == 6 ~= 7 = 8", [Number "1", Le, Number "2", Lt, Number "3", Gt, Number "4", Ge, Number "5", Ee, Number "6", Ne, Number "7", Equal, Number "8", Eof])
  , ("((1)) + (3 * (4 + 5))", [LParen, LParen, Number "1", RParen, RParen, Add, LParen, Number "3", Mult, LParen, Number "4", Add, Number "5", RParen, RParen, Eof])
  , ("proc doNothing(x, y) { x = x + 1;y=y + 2; z=(x/y)*2;}", [Proc, Ident "doNothing", LParen, Ident "x", Comma, Ident "y", RParen, LBrace, Ident "x", Equal, Ident "x", Add, Number "1", Semicolon, Ident "y", Equal, Ident "y", Add, Number "2", Semicolon, Ident "z", Equal, LParen, Ident "x", Div, Ident "y", RParen, Mult, Number "2", Semicolon, RBrace, Eof])
  , ("proc SUM (A, B){RETURN=A+B; return RETURN;}", [Proc, Ident "SUM", LParen, Ident "A", Comma, Ident "B", RParen, LBrace, Ident "RETURN", Equal, Ident "A", Add, Ident "B", Semicolon, Return, Ident "RETURN", Semicolon, RBrace, Eof])
  , ("a `plus` b", [Ident "a", InfixIdent "plus", Ident "b", Eof])
  , ("1 `!!?` 9", [Number "1", InfixIdent "!!?", Number "9", Eof])
  ]

checkParensTests =
  [ ([Eof], True)
  , ([], True)
  , ([LParen, Eof], False)
  , ([RParen, Eof], False)
  , ([LParen, Number "123", Add, Number "456", RParen, Eof], True)
  , ([LParen, Ident "a", Eof], False)
  , ([LParen, LParen, Number "1", RParen, RParen, Eof], True)
  , ([LParen, LParen, Number "1", RParen, RParen, RParen, Eof], False)
  , ([LParen, LParen, LParen, Number "1", RParen, RParen, Eof], False)
  , ([LBrace], False)
  , ([RBrace], False)
  , ([LBrace, RBrace], True)
  , ([LBrace, LParen, LBrace, RBrace, RParen, RBrace], True)
  , ([LBrace, LBrace, LBrace, RParen, RParen, RBrace], False)
  ]

toTest testFunction x =
  let (input, expected) = x
   in TestLabel ("input: " ++ show input) (TestCase $ assertEqual ("input: " ++ show input) expected (testFunction input))

tests =
  TestList
    (map (toTest tokenize) tokenizeTests ++ map (toTest checkParens) checkParensTests)

main :: IO ()
main = runTestTTAndExit tests
