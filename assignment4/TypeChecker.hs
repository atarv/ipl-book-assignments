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

import           Data.List
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
    [ ("printInt"   , ([Type_int], Type_void))
    , ("printDouble", ([Type_double], Type_void))
    , ("printString", ([Type_string], Type_void))
    , ("readInt"    , ([], Type_int))
    , ("readDouble" , ([], Type_double))
    , ("readString" , ([], Type_string))
    ]

emptyEnv :: Env
emptyEnv = Env primitiveFns [] Type_void

typecheck :: Program -> Err Program
typecheck = \case
    PDefs defs -> do
        env'         <- foldM addDef emptyEnv defs
        (_, checked) <- foldM checkDef' (env', []) defs
        pure (PDefs $ reverse checked)
      where
        checkDef' (e, dfs) def = do
            (env', checkedDef) <- checkDef e def
            pure (env', checkedDef : dfs)

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
    when (isJust $ Map.lookup name ctx)
        $  fail
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

checkDef :: Env -> Def -> Err (Env, Def)
checkDef env = \case
    def@(DFun returnType name params stms) -> do
        -- Add parameters to current context
        env' <- foldM (\e (ADecl type' id') -> updateVar e id' type')
                      (newBlock env)
                      params
        -- Check function body
        (env'', checked) <- checkStms (env' { expectedReturn = returnType })
                                      stms
        pure
            ( exitBlock $ env'' { expectedReturn = expectedReturn env' }
            , DFun returnType name params checked
            )

addDef :: Env -> Def -> Err Env
addDef env = \case
    DFun returnType name params stms -> do
        let paramTypes = fmap (\(ADecl type' _) -> type') params
        updateFn env name paramTypes returnType

checkStms :: Env -> [Stm] -> Err (Env, [Stm])
checkStms env stms = do
    result <- foldM (\(e, stms) stm -> fmap (: stms) <$> checkStm e stm)
                    (env, [])
                    stms
    pure $ fmap reverse result

checkStm :: Env -> Stm -> Err (Env, Stm)
checkStm env = \case
    SExp exp -> do
        ETyped exp' typ <- inferExp env exp
        pure (env, SExp $ ETyped exp' typ)
    stm@(SDecls type' ids) -> do
        env' <- foldM (\env' id' -> updateVar env' id' type') env ids
        pure (env', stm)
    SInit type' id' exp -> do
        ETyped exp' actual <- inferExp env exp
        unless (actual == type')
            $  fail
            $  "Expected type "
            <> printTree type'
            <> " in "
            <> printTree exp
            <> ", but got "
            <> printTree actual
        env' <- updateVar env id' type'
        pure (env', SInit type' id' (ETyped exp' actual))
    SReturn exp -> do
        ETyped exp' actual <- inferExp env exp
        let expected = expectedReturn env
        unless (actual == expected)
            $  fail
            $  "Expected "
            <> printTree expected
            <> " but got "
            <> printTree actual
            <> " in return expression "
            <> printTree exp'
        pure (env, SReturn $ ETyped exp' actual)
    SReturnVoid -> do
        let expected = expectedReturn env
        unless (expected == Type_void)
            $  fail
            $  "Expected to return "
            <> printTree expected
            <> " but got "
            <> printTree Type_void
        pure (env, SReturnVoid)
    SWhile exp stm -> do
        ETyped exp' conditionalType <- inferExp env exp
        unless (conditionalType == Type_bool)
               (conditionalTypeError conditionalType exp')
        (env', stm') <- checkStm env stm
        pure (env', SWhile (ETyped exp' conditionalType) stm')
    SBlock stms -> do
        (env', body) <- checkStms (newBlock env) stms
        pure (exitBlock env', SBlock body)
    SIfElse exp stm stm' -> do
        ETyped exp' conditionalType <- inferExp env exp
        unless (conditionalType == Type_bool)
               (conditionalTypeError conditionalType exp)
        (env' , checkedStm ) <- checkStm env stm
        (env'', checkedStm') <- checkStm env' stm'
        pure
            ( env''
            , SIfElse (ETyped exp' conditionalType) checkedStm checkedStm'
            )
  where
    conditionalTypeError actual exp =
        fail
            $  "Conditional must be of type "
            <> printTree Type_bool
            <> ", got "
            <> printTree actual
            <> " in "
            <> printTree exp
            <> "\n"
            <> show exp

inferBin :: [Type] -> Env -> Exp -> Exp -> Err (Exp, Exp)
inferBin allowedTypes env lhs rhs = do
    exp@( ETyped _ type') <- inferExp env lhs
    exp'@(ETyped _ _    ) <- inferExp env rhs
    if type' `elem` allowedTypes
        then checkExp env type' rhs
        else
            fail
            $  "Wrong type of expression "
            ++ printTree lhs
            <> ", expected "
            <> show allowedTypes
    pure (exp, exp')

inferUnary :: (Exp -> Exp) -> [Type] -> Env -> Exp -> Err Exp
inferUnary constructor allowedTypes env exp = do
    exp'@(ETyped _ type') <- inferExp env exp
    if type' `elem` allowedTypes
        then pure $ ETyped (constructor exp') type'
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
    ETyped exp type' <- inferExp env exp
    unless (type' == expected)
        $  fail
        $  "Expected: "
        <> printTree expected
        <> "\ngot: "
        <> printTree type'
        <> "\nin binary expression "
        <> printTree exp

inferExp :: Env -> Exp -> Err Exp
inferExp env exp = case exp of
    ETrue                 -> pure $ ETyped ETrue Type_bool
    EFalse                -> pure $ ETyped EFalse Type_bool
    e@(EInt    _        ) -> pure $ ETyped e Type_int
    e@(EDouble _        ) -> pure $ ETyped e Type_double
    e@(EString _        ) -> pure $ ETyped e Type_string
    e@(EId     id'      ) -> ETyped e <$> lookupVar env id'
    e@(EApp fnName exprs) -> do
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
        pure $ ETyped e returnType
    EPIncr lhs     -> inferUnary EPIncr numeric env lhs
    EPDecr lhs     -> inferUnary EPDecr numeric env lhs
    EIncr  lhs     -> inferUnary EIncr numeric env lhs
    EDecr  lhs     -> inferUnary EDecr numeric env lhs
    ETimes lhs rhs -> inferBin' ETimes lhs rhs numeric
    EDiv   lhs rhs -> inferBin' EDiv lhs rhs numeric
    EPlus  lhs rhs -> inferBin' EPlus lhs rhs plus
    EMinus lhs rhs -> inferBin' EMinus lhs rhs numeric
    ELt    lhs rhs -> inferBinComparison ELt lhs rhs
    EGt    lhs rhs -> inferBinComparison EGt lhs rhs
    ELtEq  lhs rhs -> inferBinComparison ELtEq lhs rhs
    EGtEq  lhs rhs -> inferBinComparison EGtEq lhs rhs
    EEq    lhs rhs -> inferBinComparison EEq lhs rhs
    ENEq   lhs rhs -> inferBinComparison ENEq lhs rhs
    EAnd   lhs rhs -> do
        (lhs'@(ETyped _ type'), rhs') <- inferBin [Type_bool] env lhs rhs
        pure $ ETyped (EAnd lhs' rhs') Type_bool
    EOr lhs rhs -> do
        (lhs'@(ETyped _ type'), rhs') <- inferBin [Type_bool] env lhs rhs
        pure $ ETyped (EOr lhs' rhs') Type_bool
    EAss lhs rhs -> do
        ETyped lhs' lhsType <- inferExp env (getId lhs)
        ETyped rhs' rhsType <- inferExp env rhs
        if lhsType == rhsType
            then pure $ ETyped (EAss lhs' rhs') lhsType
            else
                fail
                $  "Cannot assign value of type "
                <> printTree rhsType
                <> " to variable "
                <> printTree lhs
                <> " of type "
                <> printTree lhsType
    ETyped _exp _ty -> pure exp
  where
    numeric    = [Type_int, Type_double]
    plus       = Type_string : numeric
    comparable = [Type_bool, Type_string] <> numeric
    inferBin' constructor lhs rhs allowedTypes = do
        (lhs'@(ETyped _ type'), rhs') <- inferBin allowedTypes env lhs rhs
        pure $ ETyped (constructor lhs' rhs') type'
    inferBinComparison constructor lhs rhs = do
        (lhs', rhs') <- inferBin comparable env lhs rhs
        pure $ ETyped (constructor lhs' rhs') Type_bool
    getId id'@(EId _) = id'
    getId (ETyped id'@(EId _) _) = id'
    getId exp = error $ "Expected identifier but got " <> printTree exp

parse :: String -> Err Program
parse = pProgram . myLexer
