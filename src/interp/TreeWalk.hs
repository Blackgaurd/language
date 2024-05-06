module TreeWalk where

import qualified Ast
import qualified Data.Map as Map
import GHC.Integer (divInteger)
import qualified LangUtils
import qualified Lexer
import qualified ParseProg

{- INTERPRETER TYPES -}
data Value = Num Integer | Boolean Bool | Void deriving (Show)
type VarEnv = Map.Map Ast.Identifier Value
type ProcEnv = Map.Map Ast.Identifier Ast.Procedure
type Builtin = [Value] -> IO Value

instance Eq Value where
  (Num a) == (Num b) = a == b
  (Boolean a) == (Boolean b) = a == b
  a == b = error ("== not defined for " ++ show a ++ " and " ++ show b)

instance Ord Value where
  compare (Num a) (Num b) = compare a b
  compare a b = error ("ordering not defiend for " ++ show a ++ " and " ++ show b)

{- BUILTINS -}
builtins :: Map.Map Ast.Identifier Builtin
builtins = Map.fromList [("!disp", dispValue)]

isBuiltin :: Ast.Identifier -> Bool
isBuiltin name = Map.member name builtins

isTruthy :: Value -> Bool
isTruthy (Boolean b) = b
isTruthy x = error ("expected boolean value, got=" ++ show x)

dispValue :: [Value] -> IO Value
dispValue [] = putStrLn "" >> return Void
dispValue [Num x] = print x >> return Void
dispValue [Boolean b] = putStrLn (if b then "!t" else "!f") >> return Void
dispValue [Void] = putStrLn "<void>" >> return Void
dispValue _ = error "disp only takes one argument"

{- TREE WALKING INTERPRETER -}
numBinOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
numBinOp op (Num a) (Num b) = Num (op a b)
numBinOp _ l r = error ("arithmetic infix operator not supported for lhs=" ++ show l ++ ", rhs=" ++ show r)

logicalBinOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
logicalBinOp op (Boolean a) (Boolean b) = Boolean (op a b)
logicalBinOp _ l r = error ("logical infix operator not supported for lhs=" ++ show l ++ ", rhs=" ++ show r)

binOpTrans :: Ast.BinOp -> (Value -> Value -> Value)
binOpTrans Ast.Add = numBinOp (+)
binOpTrans Ast.Sub = numBinOp (-)
binOpTrans Ast.Mult = numBinOp (*)
binOpTrans Ast.Div = numBinOp divInteger
binOpTrans Ast.Lt = \a b -> Boolean ((<=) a b)
binOpTrans Ast.Gt = \a b -> Boolean ((>=) a b)
binOpTrans Ast.Le = \a b -> Boolean ((<) a b)
binOpTrans Ast.Ge = \a b -> Boolean ((>) a b)
binOpTrans Ast.Ee = \a b -> Boolean ((==) a b)
binOpTrans Ast.Ne = \a b -> Boolean ((/=) a b)
binOpTrans Ast.LAnd = logicalBinOp (&&)
binOpTrans Ast.LOr = logicalBinOp (||)

numUnOp :: (Integer -> Integer) -> Value -> Value
numUnOp op (Num a) = Num (op a)
numUnOp _ r = error ("arithmetic prefix operator not supported for rhs=" ++ show r)

logicalUnOp :: (Bool -> Bool) -> Value -> Value
logicalUnOp op (Boolean a) = Boolean (op a)
logicalUnOp _ r = error ("logical prefix operator not supported for rhs=" ++ show r)

unOpTrans :: Ast.UnOp -> (Value -> Value)
unOpTrans Ast.Pos = numUnOp id
unOpTrans Ast.Neg = numUnOp negate
unOpTrans Ast.LNot = logicalUnOp not

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
interpExpr _ _ (Ast.Boolean val) = return (Boolean val)
interpExpr varEnv procEnv (Ast.Var name) =
  if isBuiltin name || Map.member name procEnv
    then error (name ++ " is a builtin or procedure")
    else case Map.lookup name varEnv of
      Nothing -> error ("undefined variable: " ++ name ++ " map=" ++ show varEnv)
      Just val -> return val

-- bool in return value is returned?
interpBlock :: VarEnv -> ProcEnv -> [Ast.Stmt] -> IO (Bool, Value)
interpBlock _ _ [] = return (False, Void)
interpBlock varEnv procEnv (stmt : stmts) =
  let interpExprEnv = interpExpr varEnv procEnv
      interpBlockEnv = interpBlock varEnv procEnv
   in case stmt of
        (Ast.Set name expr) ->
          interpExprEnv expr >>= \val ->
            let newVarEnv = Map.insert name val varEnv
             in interpBlock newVarEnv procEnv stmts
        (Ast.Eval expr) -> interpExprEnv expr >> interpBlockEnv stmts
        (Ast.Return Nothing) -> return (True, Void)
        (Ast.Return (Just expr)) -> interpExprEnv expr >>= \retVal -> return (True, retVal)
        (Ast.BlockStmt innerStmts) -> interpBlockEnv innerStmts >>= \(returned, retVal) -> if returned then return (True, retVal) else interpBlockEnv stmts
        (Ast.When expr tStmts) -> interpExprEnv expr >>= \check -> if isTruthy check then interpBlockEnv (Ast.BlockStmt tStmts : stmts) else interpBlockEnv stmts
        (Ast.WhenOtherwise expr tStmts fStmts) -> interpExprEnv expr >>= \check -> if isTruthy check then interpBlockEnv (Ast.BlockStmt tStmts : stmts) else interpBlockEnv (Ast.BlockStmt fStmts : stmts)

interpProc :: VarEnv -> ProcEnv -> Ast.Procedure -> [Value] -> IO Value
interpProc varEnv procEnv (Ast.Proc params body) args =
  -- NOTE: varEnv *should* be empty, but keep just in case
  if length params /= length args
    then error "wrong number of arguments"
    else
      -- Map.union prefers elements from the first list
      let newVarEnv = Map.union (Map.fromList (zip params args)) varEnv
       in interpBlock newVarEnv procEnv body >>= \(_, val) -> return val

interpProg :: Ast.Program -> IO Value
interpProg (Ast.Prog procedures) =
  if LangUtils.hasDuplicates (map fst procedures)
    then
      error "duplicate procedure names"
    else
      let varEnv = Map.empty
          procEnv = Map.fromList procedures
          mainProc = Map.lookup "main" procEnv
       in case mainProc of
            Nothing -> error "no main procedure found"
            Just mP -> interpProc varEnv procEnv mP []

interpString :: String -> IO Value
interpString source =
  let toks = Lexer.tokenize source
      progAst = ParseProg.parseProg toks
   in interpProg progAst
