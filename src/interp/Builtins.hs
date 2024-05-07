module Builtins where

import qualified Ast
import qualified Data.Map as Map
import Value

type Builtin = [Value] -> IO Value

builtins :: Map.Map Ast.Identifier Builtin
builtins = Map.fromList [("!disp", dispWith putStr), ("!displn", dispWith putStrLn)]

isBuiltin :: Ast.Identifier -> Bool
isBuiltin name = Map.member name builtins

dispWith :: (String -> IO ()) -> [Value] -> IO Value
dispWith printer vals = printer (unwords (map valueToString vals)) >> return Void
