{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Interpreter where

import           AbsCPP
import           ErrM
import           PrintCPP

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Debug.Trace
import           Language.Haskell.TH            ( Dec(ValD) )

type Functions = Map Id Def
type Context = Map Id Val
data Val
    = ValInt Integer
    | ValBool Bool
    | ValString String
    | ValDouble Double
    | Void
    deriving (Show, Eq, Ord)

data Env = Env
    { functions :: Functions
    , contexts  :: [Context]
    , returnVal :: Val
    , returning :: Bool
    }

showVal :: Val -> String
showVal = \case
    ValInt    n     -> show n
    ValBool   True  -> "true"
    ValBool   False -> "false"
    ValString s     -> "\"" <> s <> "\""
    ValDouble x     -> show x
    Void            -> ""

primitiveFns :: Map Id Def
primitiveFns = Map.fromList
    [ ( "printInt"
      , DFun Type_void
             "printInt"
             [ADecl Type_int "i"]
             [SExp $ EApp "__print_int" [EId "i"]]
      )
    , ("readInt", DFun Type_int "readInt" [] [SReturn $ EApp "__read_int" []])
    ]

emptyEnv :: Env
emptyEnv = Env primitiveFns [Map.empty] Void False

updateFunction :: Env -> Def -> IO Env
updateFunction env def@(DFun _ name _ _) = do
    pure $ env { functions = Map.insert name def (functions env) }

addVar :: Env -> Id -> Val -> IO Env
addVar env@Env { contexts = [] }         id' val = error "Empty context"
addVar env@Env { contexts = ctx : ctxs } id' val = do
    pure $ env { contexts = Map.insert id' val ctx : ctxs }

updateVar :: Env -> Id -> Val -> IO Env
updateVar env@Env { contexts = [] } var val =
    error "No context. This shouldn't happen."
updateVar env var val = do
    let updatedContexts = updateCtx var val [] (contexts env)
    pure $ env { contexts = updatedContexts }
  where
    updateCtx var val looked []           = looked
    updateCtx var val looked (ctx : ctxs) = do
        case Map.lookup var ctx of
            Nothing -> updateCtx var val (looked <> [ctx]) ctxs
            Just _  -> looked <> (Map.insert var val ctx : ctxs)


lookupFunction :: Env -> Id -> IO Def
lookupFunction env name = case Map.lookup name (functions env) of
    Just fnDef -> pure fnDef
    Nothing    -> fail $ "Couldn't find function " <> show name

lookupVar :: Env -> Id -> IO Val
lookupVar env var =
    foldl (\val ctx -> val <|> Map.lookup var ctx) Nothing (contexts env)
        & \case
              Just val -> pure val
              Nothing  -> fail $ "Couldn't find variable " <> show var

newBlock :: Env -> Env
newBlock env = env { contexts = Map.empty : contexts env }

exitBlock :: Env -> Env
exitBlock env = env { contexts = tail (contexts env) }

evalError :: (Show a1, Show a2) => String -> a1 -> a2 -> a3
evalError opName l r =
    error
        $  "Cannot evaluate "
        <> opName
        <> " with "
        <> show l
        <> " and "
        <> show r

addVal :: Val -> Val -> Val
addVal (ValDouble x) (ValDouble y) = ValDouble (x + y)
addVal (ValInt    x) (ValInt    y) = ValInt (x + y)
addVal (ValString x) (ValString y) = ValString (x ++ y)
addVal a             b             = evalError "addVal" a b

subVal :: Val -> Val -> Val
subVal (ValDouble x) (ValDouble y) = ValDouble (x - y)
subVal (ValInt    x) (ValInt    y) = ValInt (x - y)
subVal a             b             = evalError "subVal" a b

incVal :: Val -> Val
incVal d@(ValDouble _) = addVal (ValDouble 1) d
incVal d@(ValInt    _) = addVal (ValInt 1) d
incVal d               = addVal Void d

decVal :: Val -> Val
decVal d@(ValDouble _) = subVal d (ValDouble 1)
decVal d@(ValInt    _) = subVal d (ValInt 1)
decVal d               = subVal Void d

eval :: Env -> Exp -> IO (Env, Val)
eval env = \case
    ETrue      -> pure (env, ValBool True)
    EFalse     -> pure (env, ValBool False)
    EInt    n  -> pure (env, ValInt n)
    EDouble x  -> pure (env, ValDouble x)
    EString s  -> pure (env, ValString s)
    EId     id -> do
        val <- lookupVar env id
        -- putStrLn $ show id <> ": " <> show val
        pure $ (,) env val
    -- Primitive functions
    EApp "__print_int" [int] -> do
        (env', i) <- eval env int
        putStrLn (showVal i)
        pure (env', Void)
    EApp "__read_int" [] -> do
        i <- read @Integer <$> getLine
        pure (env, ValInt i)
    EApp id exps -> do
        DFun _ _ params body <- lookupFunction env id
        let fnEnv = newBlock env
        env'  <- foldM evalArg fnEnv (zip exps params)
        env'' <- execStms env' body
        pure (exitBlock env'', returnVal env'')
      where
        evalArg env (exp, ADecl _ id') = do
            (env', val) <- eval env exp
            addVar env' id' val
    EPIncr (EId id') -> modifyPostOp id' incVal
    EPIncr exp       -> fail $ "Cannot post increment " <> printTree exp
    EPDecr (EId id') -> modifyPostOp id' decVal
    EPDecr exp       -> fail $ "Cannot post decrement " <> printTree exp
    EIncr  (EId id') -> modifyPreOp id' incVal
    EIncr  exp       -> fail $ "Cannot increment " <> printTree exp
    EDecr  (EId id') -> modifyPreOp id' decVal
    EDecr  exp       -> fail $ "Cannot decrement " <> printTree exp
    ETimes exp exp'  -> binOpL times env exp exp'
      where
        times (ValDouble x) (ValDouble y) = ValDouble (x * y)
        times (ValInt    x) (ValInt    y) = ValInt (x * y)
        times _             _             = undefined
    EDiv exp exp' -> binOpL divide env exp exp'
      where
        divide (ValDouble x) (ValDouble y) = ValDouble (x / y)
        divide (ValInt    x) (ValInt    y) = ValInt (x `div` y)
        divide _             _             = undefined
    EPlus  exp exp' -> binOpL addVal env exp exp'
    EMinus exp exp' -> binOpL subVal env exp exp'
    ELt    exp exp' -> compareExp (<) env exp exp'
    EGt    exp exp' -> compareExp (>) env exp exp'
    ELtEq  exp exp' -> compareExp (<=) env exp exp'
    EGtEq  exp exp' -> compareExp (>=) env exp exp'
    EEq    exp exp' -> compareExp (==) env exp exp'
    ENEq   exp exp' -> compareExp (/=) env exp exp'
    EAnd   exp exp' -> do
        (env', l) <- eval env exp
        if l == ValBool False then pure (env', l) else eval env' exp'
    EOr exp exp' -> do
        (env', l) <- eval env exp
        if l == ValBool True then pure (env', l) else eval env' exp'
    EAss (EId id') exp -> do
        (env', r) <- eval env exp
        env''     <- updateVar env' id' r
        pure (env'', r)
    EAss invalid exp ->
        fail $ "Cannot assign to non lvalue " <> printTree invalid
    ETyped exp ty -> eval env exp
  where
    invalidExpr exp val =
        error
            $  "Invalid expression:\n"
            <> printTree exp
            <> "\nresult: "
            <> show val
    modifyPostOp id' op = do
        val  <- lookupVar env id'
        env' <- updateVar env id' (op val)
        pure (env', val)
    modifyPreOp id' op = do
        val  <- op <$> lookupVar env id'
        env' <- updateVar env id' val
        pure (env', val)
    binOpL op env lhs rhs = do
        (env' , l) <- eval env lhs
        (env'', r) <- eval env' rhs
        pure (env'', op l r)
    compareExp op = binOpL (\x y -> ValBool $ op x y)

execStm :: Env -> Stm -> IO Env
execStm env = \case
    SExp exp         -> fst <$> eval env exp
    SDecls type' ids -> do
        foldM (\env' id' -> addVar env' id' defaultVal) env ids
      where
        defaultVal = case type' of
            Type_bool   -> ValBool False
            Type_int    -> ValInt 0
            Type_double -> ValDouble 0
            Type_void   -> Void
            Type_string -> ValString ""
    SInit ty id' exp -> do
        (env', val) <- eval env exp
        addVar env' id' val
    SReturn exp -> do
        (env', val) <- eval env exp
        pure $ env' { returnVal = val, returning = True }
    SReturnVoid    -> pure $ env { returnVal = Void, returning = True }
    SWhile exp stm -> loop env
      where
        loop env' = do
            (env'', val) <- eval env' exp
            if val == ValBool True
                then execStm env'' stm >>= loop
                else pure env''
    SBlock stms          -> exitBlock <$> execStms (newBlock env) stms
    SIfElse exp stm stm' -> do
        (env', val) <- eval env exp
        execStm env' (if val == ValBool True then stm else stm')

execStms :: Env -> [Stm] -> IO Env
execStms = foldM execStm

interpret :: Program -> IO ()
interpret (PDefs defs) = do
    env'                <- foldM updateFunction emptyEnv defs
    DFun _ _ _ mainBody <- lookupFunction env' "main"
    void $ execStms emptyEnv mainBody
