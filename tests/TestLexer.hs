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
  , ("   1234  ", [Number "1234", Eof])
  , ("AbFwdd adja", [Ident "AbFwdd", Ident "adja", Eof])
  , ("1 + 2 * four - +orange  / 9", [Number "1", Add, Number "2", Mult, Ident "four", Sub, Add, Ident "orange", Div, Number "9", Eof])
  , ("(((1)))", [LBracket, LBracket, LBracket, Number "1", RBracket, RBracket, RBracket, Eof])
  , ("(+1)-(+  2)", [LBracket, Add, Number "1", RBracket, Sub, LBracket, Add, Number "2", RBracket, Eof])
  , ("1 <= 2 < 3 > 4 >= 5 == 6 ~= 7 = 8", [Number "1", Le, Number "2", Lt, Number "3", Gt, Number "4", Ge, Number "5", Ee, Number "6", Ne, Number "7", Equal, Number "8", Eof])
  , ("((1)) + (3 * (4 + 5))", [LBracket, LBracket, Number "1", RBracket, RBracket, Add, LBracket, Number "3", Mult, LBracket, Number "4", Add, Number "5", RBracket, RBracket, Eof])
  , ("proc doNothing(x, y) { x = x + 1;y=y + 2; z=(x/y)*2;}", [Proc, Ident "doNothing", LBracket, Ident "x", Comma, Ident "y", RBracket, LBrace, Ident "x", Equal, Ident "x", Add, Number "1", Semicolon, Ident "y", Equal, Ident "y", Add, Number "2", Semicolon, Ident "z", Equal, LBracket, Ident "x", Div, Ident "y", RBracket, Mult, Number "2", Semicolon, RBrace, Eof])
  , ("proc SUM (A, B){RETURN=A+B; return RETURN;}", [Proc, Ident "SUM", LBracket, Ident "A", Comma, Ident "B", RBracket, LBrace, Ident "RETURN", Equal, Ident "A", Add, Ident "B", Semicolon, Return, Ident "RETURN", Semicolon, RBrace, Eof])
  ]

checkBracketsTests =
  [ ([Eof], True)
  , ([], True)
  , ([LBracket, Eof], False)
  , ([RBracket, Eof], False)
  , ([LBracket, Number "123", Add, Number "456", RBracket, Eof], True)
  , ([LBracket, Ident "a", Eof], False)
  , ([LBracket, LBracket, Number "1", RBracket, RBracket, Eof], True)
  , ([LBracket, LBracket, Number "1", RBracket, RBracket, RBracket, Eof], False)
  , ([LBracket, LBracket, LBracket, Number "1", RBracket, RBracket, Eof], False)
  , ([LBrace], False)
  , ([RBrace], False)
  , ([LBrace, RBrace], True)
  , ([LBrace, LBracket, LBrace, RBrace, RBracket, RBrace], True)
  , ([LBrace, LBrace, LBrace, RBracket, RBracket, RBrace], False)
  ]

toTest testFunction x =
  let (input, expected) = x
   in TestLabel ("input: " ++ show input) (TestCase $ assertEqual ("input: " ++ show input) expected (testFunction input))

tests =
  TestList
    (map (toTest tokenize) tokenizeTests ++ map (toTest checkBrackets) checkBracketsTests)

main :: IO ()
main = runTestTTAndExit tests
