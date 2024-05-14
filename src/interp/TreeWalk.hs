module TreeWalk where

import qualified Ast
import qualified Builtins
import qualified Data.Map as Map
import qualified Environment as Env
import qualified LangUtils
import qualified Lexer
import qualified ParseProg
import Value

{- TREE WALKING INTERPRETER -}
-- boolean in return is whether or not to short ciruit,
-- and identity value for if short circuit is performed
binOpTrans :: Env.ProcEnv -> Ast.BinOp -> (Maybe Bool, Value -> Value -> IO Value)
binOpTrans _ Ast.Add = (Nothing, \l r -> return (($+) l r))
binOpTrans _ Ast.Sub = (Nothing, \l r -> return (($-) l r))
binOpTrans _ Ast.Mult = (Nothing, \l r -> return (($*) l r))
binOpTrans _ Ast.Div = (Nothing, \l r -> return (($/) l r))
binOpTrans _ Ast.Mod = (Nothing, \l r -> return (($%) l r))
binOpTrans _ Ast.Gt = (Nothing, \l r -> return (($>) l r))
binOpTrans _ Ast.Lt = (Nothing, \l r -> return (($<) l r))
binOpTrans _ Ast.Ge = (Nothing, \l r -> return (($>=) l r))
binOpTrans _ Ast.Le = (Nothing, \l r -> return (($<=) l r))
binOpTrans _ Ast.Ee = (Nothing, \l r -> return (($==) l r))
binOpTrans _ Ast.Ne = (Nothing, \l r -> return (($~=) l r))
binOpTrans _ Ast.LAnd = (Just True, \l r -> return (($&) l r))
binOpTrans _ Ast.LOr = (Just False, \l r -> return (($|) l r))
binOpTrans _ Ast.At = (Nothing, \l r -> return (($@) l r))
binOpTrans procEnv (Ast.InfixIdent name) =
  case Map.lookup name procEnv of
    Nothing -> error ("can not find infix operator: " ++ name)
    Just proc -> (Nothing, \lArg rArg -> interpProc procEnv proc [lArg, rArg])

unOpTrans :: Ast.UnOp -> (Value -> Value)
unOpTrans Ast.Pos = valuePos
unOpTrans Ast.Neg = valueNot
unOpTrans Ast.LNot = valueNot

interpExpr :: Env.VarEnv -> Env.ProcEnv -> Ast.Expr -> IO Value
interpExpr varEnv procEnv (Ast.Bin op lhs rhs) =
  -- scIdent = short circuit ident?
  let (scIdent, vOp) = binOpTrans procEnv op
   in case scIdent of
        Nothing -> do
          l <- interpExpr varEnv procEnv lhs
          r <- interpExpr varEnv procEnv rhs
          vOp l r
        Just ident ->
          -- short circuiting
          interpExpr varEnv procEnv lhs >>= \l ->
            if isTruthy l == ident
              then interpExpr varEnv procEnv rhs >>= \r -> vOp l r
              else return l
interpExpr varEnv procEnv (Ast.Un op rhs) = do
  r <- interpExpr varEnv procEnv rhs
  return (unOpTrans op r)
interpExpr varEnv procEnv (Ast.Call name args) =
  case Map.lookup name Builtins.builtins of
    Just bProc ->
      mapM (interpExpr varEnv procEnv) args >>= \interpArgs -> bProc interpArgs
    Nothing ->
      case Map.lookup name procEnv of
        Nothing -> error ("undefined procedure: " ++ name)
        Just proc ->
          mapM (interpExpr varEnv procEnv) args >>= \interpArgs ->
            interpProc procEnv proc interpArgs
interpExpr _ _ (Ast.Num val) = return (Num (read val))
interpExpr _ _ (Ast.Boolean val) = return (Boolean val)
interpExpr _ _ (Ast.StringLit str) = return (StringLit str)
interpExpr varEnv procEnv (Ast.Var name) =
  if Builtins.isBuiltin name || Map.member name procEnv
    then error (name ++ " is a builtin or procedure")
    else case Env.findIdent varEnv name of
      Nothing -> error ("undefined variable: " ++ name ++ ", varEnv=" ++ show varEnv)
      Just val -> return val

-- bool in return value is returned?
interpBlock :: Env.VarEnv -> Env.ProcEnv -> Ast.Block -> IO (Bool, Env.VarEnv, Value)
interpBlock varEnv _ [] = return (False, varEnv, Void)
interpBlock varEnv procEnv (stmt : stmts) =
  let interpExprEnv = interpExpr varEnv procEnv
      interpBlockEnv = interpBlock varEnv procEnv
   in case stmt of
        (Ast.Set name expr) ->
          interpExprEnv expr >>= \val ->
            let newVarEnv = Env.setIdent varEnv name val
             in interpBlock newVarEnv procEnv stmts
        (Ast.Eval expr) -> interpExprEnv expr >> interpBlockEnv stmts
        (Ast.Return Nothing) -> return (True, Env.prevOrEmptyVarEnv varEnv, Void)
        (Ast.Return (Just expr)) ->
          interpExprEnv expr >>= \retVal ->
            return (True, Env.prevOrEmptyVarEnv varEnv, retVal)
        (Ast.BlockStmt innerStmts) ->
          let subEnv = Env.consVarEnv Map.empty varEnv
           in interpBlock subEnv procEnv innerStmts >>= \(returned, retEnv, retVal) ->
                if returned
                  then return (True, Env.prevOrEmptyVarEnv retEnv, retVal)
                  else interpBlock retEnv procEnv stmts
        (Ast.When expr tStmts) ->
          interpExprEnv expr >>= \check ->
            if isTruthy check
              then interpBlockEnv (Ast.BlockStmt tStmts : stmts)
              else interpBlockEnv stmts
        (Ast.WhenOtherwise expr tStmts fStmts) ->
          interpExprEnv expr >>= \check ->
            if isTruthy check
              then interpBlockEnv (Ast.BlockStmt tStmts : stmts)
              else interpBlockEnv (Ast.BlockStmt fStmts : stmts)
        (Ast.While expr wStmts) ->
          interpExprEnv expr >>= \check ->
            if isTruthy check
              then interpBlockEnv (Ast.BlockStmt wStmts : stmt : stmts)
              else interpBlockEnv stmts

interpProc :: Env.ProcEnv -> Ast.Procedure -> [Value] -> IO Value
interpProc procEnv (Ast.Proc params body) args =
  if length params /= length args
    then error "wrong number of arguments"
    else
      let argsMap = Map.fromList (zip params args)
          newVarEnv = Env.mapToVarEnv argsMap
       in interpBlock newVarEnv procEnv body >>= \(_, _, val) ->
            return val
interpProc procEnv (Ast.InfixL _ l r body) args =
  if length args /= 2
    then error "infix operators must take 2 arguments"
    else
      let argsMap = Map.fromList (zip [l, r] args)
          newVarEnv = Env.mapToVarEnv argsMap
       in interpBlock newVarEnv procEnv body >>= \(_, _, val) -> return val
interpProc procEnv (Ast.InfixR _ l r body) args =
  if length args /= 2
    then error "infix operators must take 2 arguments"
    else
      let argsMap = Map.fromList (zip [l, r] args)
          newVarEnv = Env.mapToVarEnv argsMap
       in interpBlock newVarEnv procEnv body >>= \(_, _, val) -> return val

interpProg :: Ast.Program -> IO Value
interpProg (Ast.Prog procedures) =
  if LangUtils.hasDuplicates (map fst procedures)
    then
      error "duplicate procedure names"
    else
      let procEnv = Map.fromList procedures
          mainProc = Map.lookup "main" procEnv
       in case mainProc of
            Nothing -> error "no main procedure found"
            Just mP -> interpProc procEnv mP []

interpString :: String -> IO Value
interpString source =
  let toks = Lexer.tokenize source
      progAst = ParseProg.parseProg toks
   in interpProg progAst
