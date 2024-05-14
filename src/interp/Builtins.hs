module Builtins where

import qualified Ast
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
ascii [str] =
  if LangUtils.lengthIs1 str
    then (Num . ord . head) str
    else error "!ascii input should be a string of length 1"
ascii _ = error "!ascii takes one argument"
