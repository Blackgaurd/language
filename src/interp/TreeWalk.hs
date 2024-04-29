{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module TreeWalk where

{- TREE WALKING INTERPRETER -}

import qualified Ast
import qualified Data.Map as Map
import GHC.Integer (divInteger)

data Value = Num Integer | Void deriving (Show)
type VarEnv = Map.Map Ast.Identifier Value
type ProcEnv = Map.Map Ast.Identifier Ast.Procedure

valueBinOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
valueBinOp op (Num a) (Num b) = Num (op a b)
valueBinOp _ Void _ = error "binary operator not supported for void"
valueBinOp _ _ Void = error "binary operator not supported for void"

binOpTrans :: Ast.BinOp -> (Value -> Value -> Value)
binOpTrans Ast.Add = valueBinOp (+)
binOpTrans Ast.Sub = valueBinOp (-)
binOpTrans Ast.Mult = valueBinOp (*)
binOpTrans Ast.Div = valueBinOp divInteger

valueUnOp :: (Integer -> Integer) -> Value -> Value
valueUnOp op (Num a) = Num (op a)
valueUnOp _ Void = error "unary operator not supported for void"

unOpTrans :: Ast.UnOp -> (Value -> Value)
unOpTrans Ast.Pos = valueUnOp id
unOpTrans Ast.Neg = valueUnOp negate

interpExpr :: VarEnv -> ProcEnv -> Ast.Expr -> Value
interpExpr varEnv procEnv (Ast.Bin op lhs rhs) = binOpTrans op l r
 where
  l = interpExpr varEnv procEnv lhs
  r = interpExpr varEnv procEnv rhs
interpExpr varEnv procEnv (Ast.Un op rhs) = unOpTrans op r
 where
  r = interpExpr varEnv procEnv rhs
interpExpr varEnv procEnv (Ast.Call name args) =
  case Map.lookup name procEnv of
    Nothing -> error ("undefined procedure: " ++ name)
    Just proc -> interpProc Map.empty procEnv proc (map (interpExpr varEnv procEnv) args)
interpExpr _ _ (Ast.Num val) = Num (read val)
interpExpr varEnv _ (Ast.Var name) =
  -- TODO: make sure variable isnt a proc
  case Map.lookup name varEnv of
    Nothing -> error ("undefined variable: " ++ name ++ " map=" ++ show varEnv)
    Just val -> val

interpBody :: VarEnv -> ProcEnv -> Ast.Body -> Value
interpBody _ _ [] = Void
interpBody varEnv procEnv (stmt : stmts) =
  case stmt of
    (Ast.Set name expr) ->
      let val = interpExpr varEnv procEnv expr
          newVarEnv = Map.insert name val varEnv
       in interpBody newVarEnv procEnv stmts
    (Ast.Eval _) -> interpBody varEnv procEnv stmts -- TODO: support side effects
    (Ast.Return Nothing) -> Void
    (Ast.Return (Just expr)) -> interpExpr varEnv procEnv expr

interpProc :: VarEnv -> ProcEnv -> Ast.Procedure -> [Value] -> Value
interpProc varEnv procEnv (Ast.Proc _ params body) args =
  -- NOTE: varEnv *should* be empty, but keep just in case
  if length params /= length args
    then error "wrong number of arguments"
    else
      -- Map.union prefers elements from the first list
      let newVarEnv = Map.union (Map.fromList (zip params args)) varEnv
       in interpBody newVarEnv procEnv body

interpProg :: Ast.Program -> Value
interpProg (Ast.Prog procedures) =
  let varEnv = Map.empty
      procEnv = Map.fromList (map (\p@(Ast.Proc name _ _) -> (name, p)) procedures)
      mainProc = Map.lookup "main" procEnv
   in case mainProc of
        Nothing -> error "no main procedure found"
        Just mP -> interpProc varEnv procEnv mP []
