module TreeWalk where

import qualified Ast
import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified LangUtils
import qualified Lexer
import qualified ParseProg
import Value

{- INTERPRETER TYPES -}
type VarEnv = Map.Map Ast.Identifier Value
type ProcEnv = Map.Map Ast.Identifier Ast.Procedure
type Builtin = [Value] -> IO Value

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
dispValue [StringLit str] = putStrLn (Array.elems str) >> return Void
dispValue [Void] = putStrLn "<void>" >> return Void
dispValue _ = error "disp only takes one argument"

{- TREE WALKING INTERPRETER -}
-- boolean in return is whether or not to short ciruit,
-- and identity value for if short circuit is performed
binOpTrans :: Ast.BinOp -> (Maybe Bool, Value -> Value -> Value)
binOpTrans Ast.Add = (Nothing, ($+))
binOpTrans Ast.Sub = (Nothing, ($-))
binOpTrans Ast.Mult = (Nothing, ($*))
binOpTrans Ast.Div = (Nothing, ($/))
binOpTrans Ast.Gt = (Nothing, ($>))
binOpTrans Ast.Lt = (Nothing, ($<))
binOpTrans Ast.Ge = (Nothing, ($>=))
binOpTrans Ast.Le = (Nothing, ($<=))
binOpTrans Ast.Ee = (Nothing, ($==))
binOpTrans Ast.Ne = (Nothing, ($~=))
binOpTrans Ast.LAnd = (Just True, ($&))
binOpTrans Ast.LOr = (Just False, ($|))

unOpTrans :: Ast.UnOp -> (Value -> Value)
unOpTrans Ast.Pos = valuePos
unOpTrans Ast.Neg = valueNot
unOpTrans Ast.LNot = valueNot

interpExpr :: VarEnv -> ProcEnv -> Ast.Expr -> IO Value
interpExpr varEnv procEnv (Ast.Bin op lhs rhs) =
  let (scIdent, vOp) = binOpTrans op
   in case scIdent of
        Nothing -> do
          l <- interpExpr varEnv procEnv lhs
          r <- interpExpr varEnv procEnv rhs
          return (vOp l r)
        Just ident ->
          -- short circuiting
          interpExpr varEnv procEnv lhs >>= \l ->
            if isTruthy l == ident
              then interpExpr varEnv procEnv rhs >>= \r -> return (vOp l r)
              else return l
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
interpExpr _ _ (Ast.StringLit str) = return (StringLit str)
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
