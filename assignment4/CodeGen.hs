{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE LambdaCase #-}
module CodeGen where

import           AbsCPP
import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Int
import           Data.List.NonEmpty             ( (<|)
                                                , NonEmpty(..)
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           PrintCPP

type FnType = Def

data Env = Env
    { vars       :: NonEmpty (Map Id (Int, Type))
    , functions  :: Map Id FnType
    , maxvar     :: Int
    , code       :: [Instruction]
    , labelCount :: Int
    }
    deriving Show

-- | JVM instruction
data Instruction
    = Aload Int -- ^ Load string from variable
    | Areturn
    | Astore Int -- ^Store ref in string variable
    | Bipush Int -- ^ Push one byte int onto stack
    | Dadd -- ^ Add doubles
    | Dcmpg -- ^ Compare doubles (greater than)
    | Dcmpl -- ^ Compare doubles (less than)
    | Dconst0
    | Dconst1
    | Ddiv
    | Dload Int -- ^ Load double from variable
    | Dmul
    | Dreturn
    | Dstore Int -- ^ Store ref in double variable
    | Dsub -- ^ Subtract doubles
    | Dup -- ^ Duplicate the top of stack
    | Dup2 -- ^ Duplicate double on top of stack
    | EndDir String
    | Goto Label
    | Iadd -- ^ Add integers from top of stack
    | Iconst1 -- ^ Push integer constant 1 on top of stack
    | Idiv
    | IfIcmpeq Label
    | IfIcmpge Label
    | IfIcmpgt Label
    | IfIcmple Label
    | IfIcmplt Label -- ^ Compare 2 integer values (<) from top of stack
    | IfIcmpne Label
    | Ifeq Label -- ^ Compare top of stack to 0. Move to label if true.
    | Ifge Label
    | Ifgt Label
    | Ifle Label
    | Iflt Label
    | Ifne Label
    | Iinc Int Int8
    | Iload Int -- ^ Load integer from variable
    | Imul
    | Invokestatic String String [String] String
        -- ^ class name, function name, param types, return type
    | Ireturn
    | Istore Int -- ^ Store ref in integer variable
    | Isub -- ^ Subtract integers
    | Label Label
    | Ldc Integer -- ^ Push integer literal
    | Ldc2W Double -- ^ Push double literal
    | LdcS String -- ^ Push string literal
    | LimitDir String Integer
    | Method [MethodMod] Id [JvmType] JvmType
    | Nop -- ^ Non-operation
    | Pop -- ^ Pop top of stack
    | Pop2 -- ^ Pop 2 bytes from stack
    | Return
    deriving Show

data MethodMod = Public | Static deriving Eq
instance Show MethodMod where
    show Public = "public"
    show Static = "static"

type Gen = State Env

type Label = String
type JvmType = String

className :: String
className = "Foo"

emptyEnv :: Env
emptyEnv = Env (Map.empty :| []) Map.empty 1 [] 0

emit :: Instruction -> Gen ()
emit instruction = modify' (\s -> s { code = instruction : code s })

lookupVar :: Id -> Gen (Int, Type)
lookupVar id' = do
    vars' <- gets vars
    case foldl (\val ctx -> val <|> Map.lookup id' ctx) Nothing vars' of
        Nothing -> error $ "Could not find variable " <> printTree id'
        Just i  -> pure i

lookupFn :: Id -> Gen FnType
lookupFn id' = do
    fns <- gets functions
    case Map.lookup id' fns of
        Nothing  -> error $ "Could not find function " <> show id'
        Just def -> pure def

addDef :: Def -> Gen ()
addDef def = undefined -- TODO: add function to env

addVar :: Id -> Type -> Gen ()
addVar id' type' = do
    (current :| rest) <- gets vars
    case Map.lookup id' current of
        Nothing -> do
            maxV <- gets maxvar
            modify'
                (\s -> s { vars   = Map.insert id' (maxV, type') current :| rest
                         , maxvar = maxV + sizeOf type'
                         }
                )
        Just _ -> error $ "Variable " <> printTree id' <> " is already defined"

sizeOf :: Type -> Int
sizeOf = \case
    Type_bool   -> 1
    Type_int    -> 1
    Type_double -> 2
    Type_void   -> 0
    Type_string -> 1

-- | Generate pop instruction for a value of given type
popBy :: Type -> Gen ()
popBy type' = case sizeOf type' of
    0 -> pure ()
    1 -> emit Pop
    2 -> emit Pop2
    n -> error $ "No pop instruction for size " <> show n

toJvmType :: Type -> JvmType
toJvmType = \case
    Type_bool   -> "Z"
    Type_int    -> "I"
    Type_double -> "D"
    Type_void   -> "V"
    Type_string -> "Ljava/lang/String;"

withinNewBlock :: Gen () -> Gen ()
withinNewBlock action = do
    previousVars <- gets vars
    modify' (\s -> s { vars = Map.empty <| vars s })
    action
    modify' (\s -> s { vars = previousVars })

newLabel :: Gen Label
newLabel = do
    lcount <- gets labelCount
    modify' (\s -> s { labelCount = lcount + 1 })
    pure $ "LABEL" <> show lcount

compileStm :: Stm -> Gen ()
compileStm stm = case stm of
    SExp exp -> do
        compileExp exp
        typ <- typeExp exp
        popBy typ
    SDecls ty ids    -> forM_ ids (`addVar` ty)
    SInit ty id' exp -> do
        addVar id' ty
        (var, type') <- lookupVar id'
        compileExp (ETyped (EAss (EId id') exp) ty)
        popBy type'
    SReturn exp -> do
        compileExp exp
        typeExp exp >>= \case
            Type_bool   -> emit Ireturn
            Type_int    -> emit Ireturn
            Type_double -> emit Dreturn
            Type_string -> emit Areturn
            Type_void   -> emit Return
    SReturnVoid     -> emit Return
    SWhile exp stm' -> do
        test <- newLabel
        end  <- newLabel
        emit (Label test)
        compileExp exp
        emit (Ifeq end)
        compileStm stm'
        emit (Goto test)
        emit (Label end)
    SBlock stms           -> withinNewBlock $ mapM_ compileStm stms
    SIfElse exp stm' stm2 -> do
        false <- newLabel
        true  <- newLabel
        compileExp exp
        emit (Ifeq false)
        compileStm stm'
        emit (Goto true)
        emit (Label false)
        compileStm stm2
        emit (Label true)

-- | Get the type of an expression
typeExp :: Exp -> Gen Type
typeExp (ETyped _ typ) = pure typ
typeExp (EInt    _   ) = pure Type_int
typeExp (EDouble _   ) = pure Type_double
typeExp (EString _   ) = pure Type_string
typeExp EFalse         = pure Type_bool
typeExp ETrue          = pure Type_bool
typeExp (EId id')      = snd <$> lookupVar id'
typeExp exp =
    error $ "Could not get type of " <> printTree exp <> "\n" <> show exp

getId :: Exp -> Id
getId (ETyped (EId id') _) = id'
getId (EId id') = id'
getId exp = error $ "Expected identifier but got " <> printTree exp

compileExp :: Exp -> Gen ()
compileExp = \case
    ETrue       -> emit (Ldc 1)
    EFalse      -> emit (Ldc 0)
    EInt    n   -> emit (Ldc n)
    EDouble x   -> emit (Ldc2W x)
    EString s   -> emit (LdcS s)
    EId     id' -> do
        (var, type') <- lookupVar id'
        case type' of
            Type_bool   -> emit (Iload var)
            Type_int    -> emit (Iload var)
            Type_double -> emit (Dload var)
            Type_string -> emit (Aload var)
            Type_void   -> error "No value for void variable"
    EApp id'@(Id name) args -> case name of
        fn@"readInt"     -> primitiveCall fn [] Type_int
        fn@"readDouble"  -> primitiveCall fn [] Type_double
        fn@"readString"  -> primitiveCall fn [] Type_string
        fn@"printString" -> primitiveCall fn [Type_string] Type_void
        fn@"printInt"    -> primitiveCall fn [Type_int] Type_void
        fn@"printDouble" -> primitiveCall fn [Type_double] Type_void
        _                -> do
            mapM_ compileExp args
            DFun returnType (Id name) params _ <- lookupFn id'
            let paramTypes = fmap (\(ADecl t argName) -> t) params
            emit
                (Invokestatic className
                              name
                              (fmap toJvmType paramTypes)
                              (toJvmType returnType)
                )
      where
        -- | Primitive functions come from a separate class so they need special
        -- handling
        primitiveCall name params retType = do
            mapM_ compileExp args
            emit
                (Invokestatic "Runtime"
                              name
                              (fmap toJvmType params)
                              (toJvmType retType)
                )
    EPIncr exp -> do
        (var, type') <- lookupVar (getId exp)
        case type' of
            Type_int -> do
                emit (Iload var)
                -- emit (Iinc var 1)
                emit Dup
                emit Iconst1
                emit Iadd
                emit (Istore var)
            Type_double -> do
                emit (Dload var)
                emit Dup2
                emit Dconst1
                emit Dadd
                emit (Dstore var)
            t -> error $ "No post increment for " <> show t
    EPDecr exp -> do
        (var, type') <- lookupVar (getId exp)
        case type' of
            Type_int -> do
                emit (Iload var)
                emit Dup
                emit Iconst1
                emit Isub
                emit (Istore var)
            Type_double -> do
                emit (Dload var)
                emit Dup2
                emit Dconst1
                emit Dsub
                emit (Dstore var)
            t -> error $ "No post decrement for type " <> show t
    EIncr exp -> do
        (var, type') <- lookupVar (getId exp)
        case type' of
            Type_int -> do
                emit (Iload var)
                emit (Ldc 1)
                emit Iadd
                emit Dup
                emit (Istore var)
            Type_double -> do
                emit (Dload var)
                emit (Ldc2W 1)
                emit Dadd
                emit Dup
                emit (Dstore var)
            t -> error $ "No increment operation for type " <> show t
    EDecr exp -> do
        (var, type') <- lookupVar (getId exp)
        case type' of
            Type_int -> do
                emit (Iload var)
                emit (Ldc 1)
                emit Isub
                emit Dup
                emit (Istore var)
            Type_double -> do
                emit (Dload var)
                emit (Ldc2W 1)
                emit Dsub
                emit Dup
                emit (Dstore var)
            t -> error $ "No decrement operation for type " <> show t
    ETimes exp exp' -> do
        compileExp exp
        compileExp exp'
        typeExp exp >>= \case
            Type_bool   -> emit Imul
            Type_int    -> emit Imul
            Type_double -> emit Dmul
            t           -> error $ "No multiplication for " <> show t
    EDiv exp exp' -> do
        compileExp exp
        compileExp exp'
        typeExp exp >>= \case
            Type_bool   -> emit Idiv
            Type_int    -> emit Idiv
            Type_double -> emit Ddiv
            t           -> error $ "No division for " <> show t
    EPlus exp exp' -> do
        compileExp exp
        compileExp exp'
        typeExp exp >>= \case
            Type_bool   -> emit Iadd
            Type_int    -> emit Iadd
            Type_double -> emit Dadd
            Type_string ->
                let strType = "Ljava/lang/String;"
                in
                    emit
                        (Invokestatic "Runtime"
                                      "plusString"
                                      [strType, strType]
                                      strType
                        )
            Type_void -> error "No plus for void"
    EMinus exp exp' -> do
        compileExp exp
        compileExp exp'
        typeExp exp >>= \case
            Type_bool   -> emit Isub
            Type_int    -> emit Isub
            Type_double -> emit Dsub
            t           -> error $ "No subtraction for " <> show t
    ELt   exp exp' -> comparison exp exp' IfIcmplt Iflt
    EGt   exp exp' -> comparison exp exp' IfIcmpgt Ifgt
    ELtEq exp exp' -> comparison exp exp' IfIcmple Ifle
    EGtEq exp exp' -> comparison exp exp' IfIcmpge Ifge
    EEq   exp exp' -> comparison exp exp' IfIcmpeq Ifeq
    ENEq  exp exp' -> comparison exp exp' IfIcmpne Ifne
    EAnd  exp exp' -> do
        emit (Bipush 0)
        end <- newLabel
        compileExp exp
        emit (Ifeq end) -- If lhs is false -> short circuit evaluation
        compileExp exp'
        -- If rhs is also false -> jump to end so that 0 is left on stack
        emit (Ifeq end)
        emit Pop -- If jump didn't happen, pop 0 from top of stack
        emit (Bipush 1) -- and replace it with 1 (true)
        emit (Label end)
    EOr exp exp' -> do
        emit (Bipush 1)
        end <- newLabel
        compileExp exp
        emit (Ifgt end)
        compileExp exp'
        emit (Ifgt end)
        -- If neither of jumps happened, replace top of stack with 0
        emit Pop
        emit (Bipush 0)
        emit (Label end)
    EAss lhs rhs -> do
        compileExp rhs
        (var, ty) <- lookupVar (getId lhs)
        case ty of
            Type_bool -> do
                emit Dup
                emit (Istore var)
            Type_int -> do
                emit Dup
                emit (Istore var)
            Type_double -> do
                emit Dup2
                emit (Dstore var)
            Type_string -> do
                emit Dup
                emit (Astore var)
            Type_void -> error "No value for void variable"
    ETyped exp _ty -> compileExp exp
  where
    comparison exp exp' intCmp doubleCmp = do
        emit (Bipush 1) -- Put 1 on top of stack
        compileExp exp
        compileExp exp'
        true <- newLabel
        typeExp exp >>= \case
            Type_double -> do
                emit Dcmpg -- Compare expression results
                -- use if** instruction to determine whether to jump
                emit (doubleCmp true)
            Type_string -> do
                emit
                    (Invokestatic
                        "Runtime"
                        "stringCompare"
                        (fmap toJvmType [Type_string, Type_string])
                        (toJvmType Type_int)
                    )
                emit (doubleCmp true)
            Type_void -> error "Cannot compare void"
            _         -> emit (intCmp true)
        emit Pop -- If jump didn't happen, the first 1 is removed from stack
        emit (Bipush 0) -- and replaced with zero to indicate false
        emit (Label true)

compileDef :: Def -> Gen ()
compileDef (DFun returnType id' params body) = do
    let paramTypes = fmap (\(ADecl t _) -> t) params
    emit
        (Method [Public, Static]
                id'
                (fmap toJvmType paramTypes)
                (toJvmType returnType)
        )
    -- TODO: track locals and stack size instead of stupid constants
    emit (LimitDir "stack" 1000)
    emit (LimitDir "locals" 1000)
    mapM_ (\(ADecl t id') -> addVar id' t) params
    mapM_ compileStm                       body
    instructions <- gets code
    let hasReturn = case head instructions of
            Return  -> True
            Ireturn -> True
            Dreturn -> True
            _       -> False
    when (null body || not hasReturn) (emit Return)
    emit (EndDir "method")

compileProgram :: Program -> String
compileProgram (PDefs defs) = flip evalState emptyEnv $ do
    mapM_ compileDef defs
    (classFileTemplate <>) . unlines . fmap assemble . reverse <$> gets code

-- | Turn instructions to Jasmin assembly
assemble :: Instruction -> String
assemble = \case
    Aload n        -> "  aload " <> show n
    Areturn        -> "  areturn"
    Astore n       -> "  astore " <> show n
    Bipush n       -> "  bipush " <> show n
    Dadd           -> "  dadd"
    Dcmpg          -> "  dcmpg"
    Dcmpl          -> "  dcpml"
    Dconst0        -> "  dconst_0"
    Dconst1        -> "  dconst_1"
    Ddiv           -> "  ddiv"
    Dload n        -> "  dload " <> show n
    Dmul           -> "  dmul"
    Dreturn        -> "  dreturn"
    Dstore n       -> "  dstore " <> show n
    Dsub           -> "  dsub"
    Dup            -> "  dup"
    Dup2           -> "  dup2"
    EndDir s       -> ".end " <> s
    Goto   s       -> "  goto " <> s
    Iadd           -> "  iadd"
    Idiv           -> "  idiv"
    IfIcmpeq label -> "  if_icmpeq " <> label
    IfIcmpge label -> "  if_icmpge " <> label
    IfIcmpgt label -> "  if_icmpgt " <> label
    IfIcmple label -> "  if_icmple " <> label
    IfIcmplt label -> "  if_icmplt " <> label
    IfIcmpne label -> "  if_icmpne " <> label
    Ifeq     s     -> "  ifeq " <> s
    Ifge     label -> "  ifge " <> label
    Ifgt     label -> "  ifgt " <> label
    Ifle     label -> "  ifle " <> label
    Iflt     label -> "  iflt " <> label
    Ifne     label -> "  ifne " <> label
    Iinc n in'     -> "  iinc " <> show n <> " " <> show in'
    Iload n        -> "  iload " <> show n
    Imul           -> "  imul"
    Invokestatic class' name params returnType ->
        "  invokestatic "
            <> class'
            <> "/"
            <> name
            <> "("
            <> mconcat params
            <> ")"
            <> returnType
    Ireturn      -> "  ireturn"
    Istore n     -> "  istore " <> show n
    Isub         -> "  isub"
    Label s      -> " " <> s <> ":"
    Ldc   n      -> "  ldc " <> show n
    Ldc2W x      -> "  ldc_2w" <> show x
    LdcS  s      -> "  ldc " <> show s
    LimitDir s n -> "  .limit " <> s <> " " <> show n
    Method _ (Id "main") _ _ ->
        ".method public static main([Ljava/lang/String;)V"
    Method mods (Id name) params return -> mconcat
        [ ".method "
        , unwords . fmap show $ mods
        , " "
        , name
        , "("
        , mconcat params
        , ")"
        , return
        ]
    Nop     -> "  nop"
    Pop     -> "  pop"
    Pop2    -> "  pop2"
    Return  -> "  return"
    Iconst1 -> "  iconst_1"

classFileTemplate :: String
classFileTemplate =
    ".class public "
        <> className
        <> "\n.super java/lang/Object\n\
    \\n\
    \.method public <init>()V\n\
    \  aload_0\n\
    \  invokespecial java/lang/Object/<init>()V\n\
    \  return\n\
    \.end method\n\
    \\n"
