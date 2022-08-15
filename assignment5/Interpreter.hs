{-# LANGUAGE LambdaCase #-}
module Interpreter (interpret, eval, EvaluationStrategy(..)) where

import           AbsFun
import           Control.Monad.State.Strict
import           Data.Function
import           Data.Functor
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Debug.Trace
import           GHC.Stack
import           Prelude                 hiding ( error )
import qualified Prelude                        ( error )
import           PrintFun

data EvaluationStrategy = CallByName | CallByValue deriving (Show, Eq)

data Env = Env
  { functions :: Map Ident Exp
  , variables :: Map Ident Exp
  , strategy  :: EvaluationStrategy
  }
  deriving (Show, Eq)

type Fun a = State Env a

emptyEnv :: EvaluationStrategy -> Env
emptyEnv = Env Map.empty Map.empty

error :: HasCallStack => String -> a
error msg = Prelude.error $ "ERROR: " <> msg

extendFn :: Def -> Fun ()
extendFn def@(DFun name params body) =
  gets functions
    >>= \case
          Just _ -> error $ show name <> " is already defined"
          Nothing ->
            modify'
              (\s ->
                s { functions = Map.insert name (toLambda def) (functions s) }
              )
    .   Map.lookup name
  where toLambda def@(DFun _ params body) = foldr ELambda body params

updateVar :: Ident -> Exp -> Fun ()
updateVar name val =
  modify' (\s -> s { variables = Map.insert name val (variables s) })

lookupFn :: Ident -> Fun Exp
lookupFn fnName =
  gets functions
    >>= \case
          Nothing  -> error $ "Function " <> show fnName <> " is not defined"
          Just def -> pure def
    .   Map.lookup fnName

lookupVar :: Ident -> Fun (Maybe Exp)
lookupVar var = gets variables <&> Map.lookup var

interpret :: EvaluationStrategy -> Program -> Integer
interpret useCallByValue = \case
  PDefs defs -> flip evalState (emptyEnv useCallByValue) $ do
    mapM_ extendFn defs
    lookupFn (Ident "main") >>= eval >>= \case
      EInt n -> pure n
      e      -> error $ "main must return an int, got " <> printTree e

eval :: Exp -> Fun Exp
eval x = case x of
    EId ident -> lookupVar ident >>= \case
      Nothing  -> lookupFn ident >>= eval
      Just exp -> eval exp
    int@(EInt integer)    -> pure int
    EApp exp1        exp2 -> eval exp1 >>= \case
      ELambda param body -> do
        argValue <- gets strategy >>= \case
          CallByName  -> pure exp2
          CallByValue -> eval exp2
        updateVar param argValue
        eval body
      unapplicable -> error $ "Cannot apply " <> printTree unapplicable
    EAdd exp1 exp2 -> do
      lhs <- eval exp1
      rhs <- eval exp2
      case (lhs, rhs) of
        (EInt a, EInt b) -> pure $ EInt (a + b)
        (a, b) ->
          error $ "Cannot add " <> printTree a <> " and " <> printTree b
    ESub exp1 exp2 -> do
      lhs <- eval exp1
      rhs <- eval exp2
      case (lhs, rhs) of
        (EInt a, EInt b) -> pure $ EInt (a - b)
        (a, b) ->
          error $ "Cannot subtract " <> printTree a <> " and " <> printTree b
    ELt exp1 exp2 -> do
      lhs <- eval exp1
      rhs <- eval exp2
      case (lhs, rhs) of
        (EInt a, EInt b) -> pure . EInt $ if a < b then 1 else 0
        (a, b) ->
          error $ "Cannot compare " <> printTree a <> " and " <> printTree b
    EIf condition consequence alternative -> eval condition >>= \case
      (EInt n) -> eval $ if n > 0 then consequence else alternative
      e ->
        error $ "Condition must evaluate to an integer " <> printTree condition
    lam@(ELambda _ident _exp) -> pure lam
