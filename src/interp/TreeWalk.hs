module TreeWalk where

import qualified Ast
import qualified Data.Map as Map
import GHC.Integer (divInteger)

{- INTERPRETER TYPES -}
data Value = Num Integer | Void deriving (Show)
type VarEnv = Map.Map Ast.Identifier Value
type ProcEnv = Map.Map Ast.Identifier Ast.Procedure
type Builtin = [Value] -> IO Value

{- BUILTINS -}
builtins :: Map.Map Ast.Identifier Builtin
builtins = Map.fromList [("disp", dispValue)]

isBuiltin :: Ast.Identifier -> Bool
isBuiltin name = Map.member name builtins

dispValue :: [Value] -> IO Value
dispValue [] = putStrLn "" >> return Void
dispValue [Num x] = print x >> return Void
dispValue [Void] = putStrLn "<void>" >> return Void
dispValue _ = error "disp only takes one argument"

{- TREE WALKING INTERPRETER -}
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

interpExpr :: VarEnv -> ProcEnv -> Ast.Expr -> IO Value
interpExpr varEnv procEnv (Ast.Bin op lhs rhs) = do
  l <- interpExpr varEnv procEnv lhs
  r <- interpExpr varEnv procEnv rhs
  return (binOpTrans op l r)
interpExpr varEnv procEnv (Ast.Un op rhs) = do
  r <- interpExpr varEnv procEnv rhs
  return (unOpTrans op r)
interpExpr varEnv procEnv (Ast.Call name args) =
  case Map.lookup name builtins of
    Just bProc ->
      mapM (interpExpr varEnv procEnv) args >>= \interpArgs -> bProc interpArgs
    Nothing ->
      case Map.lookup name procEnv of
        Nothing -> error ("undefined procedure: " ++ name)
        Just proc ->
          mapM (interpExpr varEnv procEnv) args >>= \interpArgs ->
            interpProc Map.empty procEnv proc interpArgs
interpExpr _ _ (Ast.Num val) = return (Num (read val))
interpExpr varEnv procEnv (Ast.Var name) =
  if isBuiltin name || Map.member name procEnv
    then error (name ++ " is a builtin or procedure")
    else case Map.lookup name varEnv of
      Nothing -> error ("undefined variable: " ++ name ++ " map=" ++ show varEnv)
      Just val -> return val

interpBody :: VarEnv -> ProcEnv -> Ast.Body -> IO Value
interpBody _ _ [] = return Void
interpBody varEnv procEnv (stmt : stmts) =
  case stmt of
    (Ast.Set name expr) ->
      interpExpr varEnv procEnv expr >>= \val ->
        let newVarEnv = Map.insert name val varEnv
         in interpBody newVarEnv procEnv stmts
    (Ast.Eval expr) -> interpExpr varEnv procEnv expr >> interpBody varEnv procEnv stmts
    (Ast.Return Nothing) -> return Void
    (Ast.Return (Just expr)) -> interpExpr varEnv procEnv expr

interpProc :: VarEnv -> ProcEnv -> Ast.Procedure -> [Value] -> IO Value
interpProc varEnv procEnv (Ast.Proc params body) args =
  -- NOTE: varEnv *should* be empty, but keep just in case
  if length params /= length args
    then error "wrong number of arguments"
    else
      -- Map.union prefers elements from the first list
      let newVarEnv = Map.union (Map.fromList (zip params args)) varEnv
       in interpBody newVarEnv procEnv body

interpProg :: Ast.Program -> IO Value
interpProg (Ast.Prog procedures) =
  let varEnv = Map.empty
      procEnv = Map.fromList procedures
      mainProc = Map.lookup "main" procEnv
   in case mainProc of
        Nothing -> error "no main procedure found"
        Just mP -> interpProc varEnv procEnv mP []
