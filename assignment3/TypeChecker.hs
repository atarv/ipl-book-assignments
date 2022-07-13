{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeChecker (parse, typecheck) where

import           AbsCPP
import           Control.Applicative
import           Control.Monad
import           ErrM
import           Prelude                 hiding ( exp )
import           PrintCPP

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( isJust )
import           ParCPP                         ( myLexer
                                                , pProgram
                                                )

type Context = Map Id Type
type FnSignature = ([Type], Type)
type Signatures = Map Id FnSignature
data Env = Env
    { signatures     :: Signatures
    , contexts       :: [Context]
    , expectedReturn :: Type
    }
    deriving (Show, Eq)

primitiveFns :: Signatures
primitiveFns = Map.fromList
    [("printInt", ([Type_int], Type_void)), ("readInt", ([], Type_int))]

emptyEnv :: Env
emptyEnv = Env primitiveFns [] Type_void

typecheck :: Program -> Err ()
typecheck = \case
    PDefs defs -> do
        env' <- foldM addDef emptyEnv defs
        foldM_ checkDef env' defs


lookupVar :: Env -> Id -> Err Type
lookupVar Env { contexts = [] } _ = error "Empty context"
lookupVar Env {..} name =
    case foldl (\item ctx -> item <|> Map.lookup name ctx) Nothing contexts of
        Nothing      -> fail $ "Variable '" <> show name <> "' not found"
        Just varType -> pure varType

lookupFn :: Env -> Id -> Err FnSignature
lookupFn Env {..} name = case Map.lookup name signatures of
    Nothing      -> fail $ "Function '" <> show name <> "' not found"
    Just varType -> pure varType

updateVar :: Env -> Id -> Type -> Err Env
updateVar Env { contexts = [] } _name _varType =
    fail "No context when trying to update variable type"
updateVar env@Env { contexts = ctx : ctxs } name varType = do
    when (isJust $ Map.lookup name ctx) $ do
        fail
            $  "Variable "
            <> show name
            <> " is already declared in this context"
    pure $ env { contexts = Map.insert name varType ctx : ctxs }

updateFn :: Env -> Id -> [Type] -> Type -> Err Env
updateFn env@Env {..} fnName paramTypes returnType =
    case Map.lookup fnName signatures of
        Just _  -> fail $ "Function " <> show fnName <> " is already defined"
        Nothing -> pure $ env
            { signatures = Map.insert fnName (paramTypes, returnType) signatures
            }

newBlock :: Env -> Env
newBlock env = env { contexts = Map.empty : contexts env }

exitBlock :: Env -> Env
exitBlock env = env { contexts = safeTail (contexts env) }
  where
    safeTail [] = []
    safeTail xs = tail xs

checkDef :: Env -> Def -> Err Env
checkDef env = \case
    DFun returnType name params stms -> do
        -- Add parameters to current context
        env' <- foldM (\e (ADecl type' id') -> updateVar e id' type')
                      (newBlock env)
                      params
        -- Check function body
        env'' <-
            exitBlock <$> checkStms (env' { expectedReturn = returnType }) stms
        pure $ env'' { expectedReturn = expectedReturn env' }

addDef :: Env -> Def -> Err Env
addDef env = \case
    DFun returnType name params stms -> do
        let paramTypes = fmap (\(ADecl type' _) -> type') params
        updateFn env name paramTypes returnType

checkStms :: Env -> [Stm] -> Err Env
checkStms = foldM checkStm

checkStm :: Env -> Stm -> Err Env
checkStm env = \case
    SExp exp -> do
        void $ inferExp env exp
        pure env
    SDecls type' ids    -> foldM (\env' id' -> updateVar env' id' type') env ids
    SInit type' id' exp -> do
        actual <- inferExp env exp
        unless (actual == type')
            $  fail
            $  "Expected type "
            <> printTree type'
            <> " in "
            <> printTree exp
            <> ", but got "
            <> printTree actual
        updateVar env id' type'
    SReturn exp -> do
        actual <- inferExp env exp
        let expected = expectedReturn env
        unless (actual == expected)
            $  fail
            $  "Expected "
            <> printTree expected
            <> " but got "
            <> printTree actual
            <> " in return expression "
            <> printTree exp
        pure env
    SReturnVoid -> do
        let expected = expectedReturn env
        unless (expected == Type_void)
            $  fail
            $  "Expected to return "
            <> printTree expected
            <> " but got "
            <> printTree Type_void
        pure env
    SWhile exp stm -> do
        conditionalType <- inferExp env exp
        unless (conditionalType == Type_bool)
               (conditionalTypeError conditionalType exp)
        checkStm env stm
    SBlock stms          -> exitBlock <$> checkStms (newBlock env) stms
    SIfElse exp stm stm' -> do
        conditionalType <- inferExp env exp
        unless (conditionalType == Type_bool)
               (conditionalTypeError conditionalType exp)
        env' <- checkStm env stm
        checkStm env' stm'
  where
    conditionalTypeError actual exp =
        fail
            $  "Conditional must be of type "
            <> printTree Type_bool
            <> ", got "
            <> printTree actual
            <> " in "
            <> printTree exp

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin allowedTypes env lhs rhs = do
    type' <- inferExp env lhs
    if type' `elem` allowedTypes
        then checkExp env type' rhs
        else fail $ "Wrong type of expression " ++ printTree lhs
    pure type'

inferUnary :: [Type] -> Env -> Exp -> Err Type
inferUnary allowedTypes env exp = do
    type' <- inferExp env exp
    if type' `elem` allowedTypes
        then pure type'
        else
            fail
            $  "Expected an expression of type "
            <> mconcat (fmap printTree allowedTypes)
            <> ", but got "
            <> printTree type'
            <> " in unary expression "
            <> printTree exp

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env expected exp = do
    actual <- inferExp env exp
    unless (actual == expected)
        $  fail
        $  "Expected: "
        <> printTree expected
        <> "\ngot: "
        <> printTree actual
        <> "\nin binary expression "
        <> printTree exp

inferExp :: Env -> Exp -> Err Type
inferExp env = \case
    ETrue             -> pure Type_bool
    EFalse            -> pure Type_bool
    EInt    _         -> pure Type_int
    EDouble _         -> pure Type_double
    EString _         -> pure Type_string
    EId     id'       -> lookupVar env id'
    EApp fnName exprs -> do
        (params, returnType) <- lookupFn env fnName
        let paramCount = length params
            argCount   = length exprs
        unless (paramCount == argCount)
            $  fail
            $  "Function "
            <> show fnName
            <> " expects "
            <> show paramCount
            <> " arguments, but "
            <> show argCount
            <> " provided"
        zipWithM_ (checkExp env) params exprs
        pure returnType
    EPIncr exp              -> inferUnary numeric env exp
    EPDecr exp              -> inferUnary numeric env exp
    EIncr  exp              -> inferUnary numeric env exp
    EDecr  exp              -> inferUnary numeric env exp
    ETimes exp         exp' -> inferBin numeric env exp exp'
    EDiv   exp         exp' -> inferBin numeric env exp exp'
    EPlus  exp         exp' -> inferBin plus env exp exp'
    EMinus exp         exp' -> inferBin numeric env exp exp'
    ELt    exp         exp' -> Type_bool <$ inferBin comparable env exp exp'
    EGt    exp         exp' -> Type_bool <$ inferBin comparable env exp exp'
    ELtEq  exp         exp' -> Type_bool <$ inferBin comparable env exp exp'
    EGtEq  exp         exp' -> Type_bool <$ inferBin comparable env exp exp'
    EEq    exp         exp' -> Type_bool <$ inferBin comparable env exp exp'
    ENEq   exp         exp' -> Type_bool <$ inferBin comparable env exp exp'
    EAnd   exp         exp' -> inferBin [Type_bool] env exp exp'
    EOr    exp         exp' -> inferBin [Type_bool] env exp exp'
    EAss   id'@(EId _) exp' -> do
        lhs <- inferExp env id'
        rhs <- inferExp env exp'
        if lhs == rhs
            then pure lhs
            else
                fail
                $  "Cannot assign value of type "
                <> printTree rhs
                <> " to variable "
                <> printTree id'
                <> " of type "
                <> printTree lhs
    EAss lhs _rhs ->
        fail
            $  "Cannot assign to "
            <> printTree lhs
            <> " because it is not an lvalue"
    ETyped _exp _ty -> undefined -- internal, not relevant for this assignment
  where
    numeric    = [Type_int, Type_double]
    plus       = Type_string : numeric
    comparable = [Type_bool, Type_string] <> numeric

parse :: String -> Err Program
parse = pProgram . myLexer
