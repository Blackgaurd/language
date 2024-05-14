module Environment where

import qualified Ast
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Value

data VarEnv = VarEnv (Map.Map Ast.Identifier Value) (Maybe VarEnv) deriving (Show)
type ProcEnv = Map.Map Ast.Identifier Ast.Procedure

emptyVarEnv :: VarEnv
emptyVarEnv = VarEnv Map.empty Nothing

mapToVarEnv :: Map.Map Ast.Identifier Value -> VarEnv
mapToVarEnv mp = VarEnv mp Nothing

consVarEnv :: Map.Map Ast.Identifier Value -> VarEnv -> VarEnv
consVarEnv mp nxt = VarEnv mp (Just nxt)

prevOrEmptyVarEnv :: VarEnv -> VarEnv
prevOrEmptyVarEnv (VarEnv _ prev) =
  fromMaybe emptyVarEnv prev

curVarMap :: VarEnv -> Maybe (Map.Map Ast.Identifier Value)
curVarMap (VarEnv mp _) = Just mp

findIdent :: VarEnv -> Ast.Identifier -> Maybe Value
findIdent (VarEnv mp upper) ident =
  case Map.lookup ident mp of
    Nothing ->
      case upper of
        Nothing -> Nothing
        Just uEnv -> findIdent uEnv ident
    Just val -> Just val

setIdent :: VarEnv -> Ast.Identifier -> Value -> VarEnv
setIdent env@(VarEnv mp upper) ident val =
  case findIdent env ident of
    Nothing ->
      -- ident does not previously exist,
      -- create it in lowest scope
      let newMp = Map.insert ident val mp in VarEnv newMp upper
    Just _ ->
      -- ident previously exists,
      -- find it and update it
      findAndUpdate env
     where
      findAndUpdate :: VarEnv -> VarEnv
      findAndUpdate (VarEnv fauMp fauUpper) =
        case Map.lookup ident fauMp of
          Nothing ->
            -- not found, go up
            case fauUpper of
              Nothing -> error "setIdent, findAndUpdate reached unexpected end"
              Just fauUpper2 -> findAndUpdate fauUpper2
          Just _ ->
            -- found, update it
            let newMp = Map.insert ident val fauMp in VarEnv newMp fauUpper
