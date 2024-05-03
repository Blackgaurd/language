module TreeWalk where

import qualified Ast
import qualified Data.Map as Map
import GHC.Integer (divInteger)
import qualified Lexer
import qualified ParseProg
import qualified Utils

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
  if Utils.hasDuplicates (map fst procedures)
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
