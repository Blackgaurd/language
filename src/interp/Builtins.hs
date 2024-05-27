module Builtins where

import qualified Ast
import qualified Data.Array as Array
import Data.Char (ord)
import qualified Data.Map as Map
import qualified LangUtils
import Value

type Builtin = [Value] -> IO Value

builtins :: Map.Map Ast.Identifier Builtin
builtins =
  Map.fromList
    [ ("!disp", dispWith putStr)
    , ("!displn", dispWith putStrLn)
    , ("!type", valType)
    , ("!ascii", ascii)
    , ("!len", len)
    , ("!readln", readln)
    , ("!to_int", toInt)
    ]

isBuiltin :: Ast.Identifier -> Bool
isBuiltin name = Map.member name builtins

dispWith :: (String -> IO ()) -> Builtin
dispWith printer vals = printer (unwords (map valueToString vals)) >> return Void

valType :: Builtin
valType [] = error "!type takes one argument"
valType [val] =
  let str = case val of
        (Num _) -> "integer"
        (Boolean _) -> "boolean"
        (StringLit _) -> "string"
        Void -> "void"
   in return (StringLit (LangUtils.stringToArray str))
valType _ = error "!type takes one argument"

ascii :: Builtin
ascii [] = error "!ascii takes one argument"
ascii [StringLit lit] =
  let str = Array.elems lit
   in if LangUtils.lengthIs1 str
        then return ((Num . toInteger . ord . head) str)
        else error "!ascii input should be a string of length 1"
ascii _ = error "!ascii takes one argument"

len :: Builtin
len [] = error "!len takes one argument"
len [StringLit lit] =
  let l = toInteger (length lit)
   in return (Num l)
len _ = error "!len takes one argument"

readln :: Builtin
readln [] = do
  StringLit . LangUtils.stringToArray <$> getLine
readln _ = error "!readln takes no arguments"

toInt :: Builtin
toInt [] = error "!to_int takes one argument"
toInt [arg] =
  case arg of
    (Num _) -> return arg
    (StringLit str) -> let num = LangUtils.arrayToInteger str in return (Num num)
    (Boolean b) -> let num = if b then 1 else 0 in return (Num num)
    Void -> error "cannot convert void to integer"
toInt _ = error "!to_int takes one argument"
