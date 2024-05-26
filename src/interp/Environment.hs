module Environment where

import qualified Ast
import qualified Data.Map as Map
import Value

type VarEnv = [Map.Map Ast.Identifier Value]
type ProcEnv = Map.Map Ast.Identifier Ast.Procedure

consVarEnv :: Map.Map Ast.Identifier Value -> VarEnv -> VarEnv
consVarEnv mp nxt = mp : nxt

prevOrEmptyVarEnv :: VarEnv -> VarEnv
prevOrEmptyVarEnv [] = [Map.empty]
prevOrEmptyVarEnv [_] = [Map.empty]
prevOrEmptyVarEnv (_ : xs) = xs

findIdent :: VarEnv -> Ast.Identifier -> Maybe Value
findIdent [] _ = Nothing
findIdent (mp : mps) ident =
  case Map.lookup ident mp of
    Nothing -> findIdent mps ident
    Just val -> Just val

setIdent :: VarEnv -> Ast.Identifier -> Value -> VarEnv
setIdent [] _ _ = error "can't set ident in empty VarEnv"
setIdent varEnv@(mp : mps) ident val =
  case findIdent varEnv ident of
    Just _ ->
      case Map.lookup ident mp of
        Nothing -> mp : setIdent mps ident val
        Just _ -> let newMap = Map.insert ident val mp in newMap : mps
    Nothing -> let newMap = Map.insert ident val mp in newMap : mps
