module Main where

import Ast
import qualified Lexer
import ParseExpr
import ParseProc
import ParseProg
import ParseStmt
import Test.HUnit

parseExprTests =
  [ ("1", "1")
  , ("1 + 2", "(1+2)")
  , ("(((1)))", "1")
  , ("1 + 2 + 3", "((1+2)+3)")
  , ("1 + 2 * 3", "(1+(2*3))")
  , ("1 * 2 + 3", "((1*2)+3)")
  , ("3 + 4 * 2", "(3+(4*2))")
  , ("-(5 + 3) * 4", "((-(5+3))*4)")
  , ("(6 - 3) / (2 + 1) * 4", "(((6-3)/(2+1))*4)")
  , ("7 * (5 - (3 + 2))", "(7*(5-(3+2)))")
  , ("+8 - 3 * 2 + (1 + 1)", "(((+8)-(3*2))+(1+1))")
  , ("9 / (3 * (2 + 1))", "(9/(3*(2+1)))")
  , ("-4 + 6 / (2 + 1) * 2", "((-4)+((6/(2+1))*2))")
  , ("(3 + 5) * -(2 - 6)", "((3+5)*(-(2-6)))")
  , ("+7 - (3 * 2) + 4 / 2", "(((+7)-(3*2))+(4/2))")
  , ("-(1 + 2) * 3 + 4 / (2 - 1)", "(((-(1+2))*3)+(4/(2-1)))")
  , ("(((-(1+2))*3)+(4/(2-1)))", "(((-(1+2))*3)+(4/(2-1)))")
  , ("(1 + 2) <= (3 + i)", "((1+2)<=(3+i))")
  , ("1 == 1 | 2 ~= 3", "((1==1)|(2~=3))")
  , ("~(7 > 9) | !t", "((~(7>9))|!t)")
  , ("add(1, two, three, 4)", "add(1,two,three,4)")
  , ("procA(procB((1 + 2) / -4), procC((((1))), (((2)+(-4)))))", "procA(procB(((1+2)/(-4))),procC(1,(2+(-4))))")
  ]

parseStmtTests =
  [ ("x = 17;", "x=17;")
  , ("x = x + 1;", "x=(x+1);")
  , ("y = x + y * z;", "y=(x+(y*z));")
  , ("-1;", "(-1);")
  , ("1 - 2 - 3 + a(4);", "(((1-2)-3)+a(4));")
  , ("return;", "return;")
  , ("return a(b)+b(5);", "return (a(b)+b(5));")
  ]

parseProcTests =
  [ ("proc a(x, y, z) {return x + y - z;}", "proc a(x,y,z){return ((x+y)-z);}")
  , ("proc sum(a, b) {ret = a + b; return ret; }", "proc sum(a,b){ret=(a+b);return ret;}")
  , ("proc doNothing (b, b) {return; doNothing(5, 5); doNothing(7, 7 + 1);}", "proc doNothing(b,b){return;doNothing(5,5);doNothing(7,(7+1));}")
  ]

toTest testFunction x =
  let (input, expected) = x
   in TestLabel ("input: " ++ show input) (TestCase $ assertEqual ("input: " ++ show input) expected (testFunction input))

tests =
  TestList
    ( map (toTest (exprToString . fst . parseExpr . Lexer.tokenize)) parseExprTests
        ++ map (toTest (stmtToString . fst . parseStmt . Lexer.tokenize)) parseStmtTests
        ++ map (toTest (procToString . fst . parseProc . Lexer.tokenize)) parseProcTests
    )

main :: IO ()
main = runTestTTAndExit tests
