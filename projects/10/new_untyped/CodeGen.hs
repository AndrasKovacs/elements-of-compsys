
{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, BangPatterns #-}

module CodeGen (
      evalClass
    , evalStatement
    , evalExpr
    , runCodeGen
    , compileClasses) where

import Prelude hiding (return, and, or, not)
import Data.List hiding (and, or)

import qualified Data.Map as M
import Control.Monad.State.Strict  hiding (return)
import Control.Applicative
import Control.DeepSeq

import Control.Lens hiding (argument)
import Text.Printf 
import Data.Char

import Types

import Debug.Trace

-- CodeGen monad 

type CodeGenState = (M.Map String Type, Int, Int, [String])
type CodeGen = State CodeGenState

vars        = _1 
labelc      = _2 -- counter of unique labels   
staticStart = _3 -- pointer to first free address of static segment
output      = _4 -- list of generated code lines


-- VM command embeddings

addCmd :: String -> CodeGen ()
addCmd cmd = output %= (cmd:)

stackOp :: String -> String -> Int -> CodeGen ()
stackOp op site i = addCmd $ printf "%s %s %d" op site i

call :: String -> Int -> CodeGen ()
call fn n = addCmd $ printf "call %s %d" fn n 

newLabel :: CodeGen String 
newLabel = ("__"++) . show <$> (labelc <<%= (+1)) 

push = stackOp "push"
pop  = stackOp "pop"
add  = addCmd "add"
neg  = addCmd "neg"
sub  = addCmd "sub"
not  = addCmd "not"
eq   = addCmd "eq"
lt   = addCmd "lt"
gt   = addCmd "gt"
and  = addCmd "and"
or   = addCmd "or"

return    = addCmd "return"
label l   = addCmd $ "label " ++ l
goto l    = addCmd $ "goto " ++ l
if_goto l = addCmd $ "if-goto " ++ l 

function name n = addCmd $ printf "function %s %d" name n 

constant = "constant"
argument = "argument"
static   = "static"
temp     = "temp"
this     = "this"
that     = "that"
local    = "local"
pointer  = "pointer"

lookupVar :: String -> CodeGen Type 
lookupVar var = maybe 
    (error $ printf "variable not in scope: %s" (show var))
    pure =<< (use $ vars . at var)

varOp :: (String -> Int -> CodeGen ()) -> String -> CodeGen ()
varOp op var = lookupVar var >>= \case
    VarT StaticVar _ i -> op static i
    VarT Argument  _ i -> op argument i 
    VarT Field     _ i -> op this i
    VarT Local     _ i -> op local i
    FunT{}             -> error "cannot dereference function"

deref   = varOp push
writeTo = varOp pop


-- Codegen 

evalExpr :: Expr -> CodeGen ()
evalExpr = \case
    Lit (I n)   -> push constant (fromIntegral n) 
    Lit (B b)   -> push constant 0 >> when b not
    Lit (C c)   -> push constant (ord c)
    Var v       -> deref v

    Add a b     -> do {evalExpr a; evalExpr b; add}
    Sub a b     -> do {evalExpr a; evalExpr b; sub}
    Mul a b     -> do {evalExpr a; evalExpr b; call "Math.multiply" 2}
    Div a b     -> do {evalExpr a; evalExpr b; call "Math.divide" 2}
    And a b     -> do {evalExpr a; evalExpr b; and}
    Or  a b     -> do {evalExpr a; evalExpr b; or}
    Cmp EQ a b  -> do {evalExpr a; evalExpr b; eq}
    Cmp LT a b  -> do {evalExpr a; evalExpr b; lt}
    Cmp GT a b  -> do {evalExpr a; evalExpr b; gt}
    Neg e       -> do {evalExpr e; neg}
    Not e       -> do {evalExpr e; not}

    Index arr e -> do
        evalExpr e
        deref arr
        add
        pop pointer 1
        push that 0

    Lit (S s) -> do
        push constant (length s) 
        call "String.new" 1
        forM_ s $ \c -> do
            push constant (ord c)
            call "String.appendChar" 2

    Call f args -> do
        use (vars . at f) >>= \case
            Just (FunT _ argc) -> do
                when (argc /= length args) $ error $ printf "wrong number of arguments in: %s" (show f)
                forM_ args evalExpr
                call f argc
            _ -> error $ printf "function not in scope: %s" (show f)

    MethodCall prefix (Call f args) ->
        use (vars . at prefix) >>= \case
            Just (VarT _ clss _) -> evalExpr $ Call (clss ++ "." ++ f) (Var prefix: args)
            _ -> evalExpr $ Call (prefix ++ "." ++ f) args

    MethodCall _ _ -> error "the impossible has happened"


evalStatement :: Statement -> CodeGen ()
evalStatement = \case
    Let var exp -> do
        evalExpr exp
        writeTo var

    LetIndex (Index arr i) exp -> do
        evalExpr exp
        evalExpr i
        deref arr
        add
        pop pointer 1
        pop that 0

    LetIndex _ _ -> error "the impossible has happened"

    If cnd thn els -> do
        true <- (++"_if_true") <$> newLabel
        end  <- (++"_end_if") <$> newLabel
        evalExpr cnd
        if_goto true
        traverse (mapM_ evalStatement) els 
        goto end
        label true 
        mapM_ evalStatement thn 
        label end 

    While cnd block -> do
        begin <- (++"_while_begin") <$> newLabel
        end   <- (++"_while_end")   <$> newLabel
        label begin
        evalExpr cnd
        not
        if_goto end
        mapM_ evalStatement block
        goto begin
        label end 

    Do exp -> do
        evalExpr exp
        pop temp 0

    Return (Var "this") -> do
        push pointer 0
        return

    Return exp -> do 
        evalExpr exp
        return 

evalClass :: Class -> CodeGen ()
evalClass (Class className varDecls funs) = do
    currStatic <- use staticStart

    let uniqueMap = M.fromListWithKey (\k _ _ -> duplicateErr k)
        uniqueUnion = M.unionWith (\_ k ->  duplicateErr k) 
        duplicateErr v = error $ printf "duplicate declaration: %s" (show v)

        fields = [(name, t) | 
            (Field, t, names) <- varDecls, name <- names]

        statics = [(name, t) | 
            (StaticVar, t, names) <- varDecls, name <- names]

        fieldMap = uniqueMap [(name, VarT Field t i) |
            (i, (name, t)) <- zip [0..] fields]

        staticsMap = uniqueMap [(name, VarT StaticVar t i) | 
            (i, (name, t)) <- zip [currStatic..] statics]

        funcs = [(name, FunT site argc) |
            Function site name args _ _ <- funs,
            let argc = length args + fromEnum (site == Method)]

        qualifiedFuncs = [(className++"."++n, t) | (n, t) <- funcs]
        qualFuncMap = uniqueMap qualifiedFuncs
        classSize = length fields 


    pure $!! foldl1' uniqueUnion [qualFuncMap, fieldMap, staticsMap] -- check duplicate declarations by forcing the maps

    archive <- use vars 
    staticStart += length statics 
    vars %= uniqueUnion (uniqueMap $ funcs ++ qualifiedFuncs)


    -- spec :
        -- inside method: can call method and field unqalified
        -- inside constructor: can call method and field unqualified
        -- inside static: can only call static qualified
        -- any objects: call method on them unqalified with dot syntax


    forM_ funs $ \(Function site name args decls body) -> do
        let argStart = fromEnum (site == Method)

            argMap = uniqueMap [(n, VarT Argument t i) | 
                (i, (t, n)) <- zip [argStart..] args]

            flatDecl = [(t, name) |
                (t, names) <- decls, name <- names]

            declMap = uniqueMap [(n, VarT Local t i) | 
                (i, (t, n)) <- zip [0..] flatDecl]

        function (className ++ "." ++ name) (length flatDecl) 
        archive <- use vars 

        when (site == Constructor) $ do 
            vars %= M.union fieldMap
            push constant classSize
            call "Memory.alloc" 1
            pop pointer 0 

        when (site == Method) $ do
            vars %= M.union fieldMap
            push argument 0
            pop pointer 0

        vars %= M.union staticsMap
        vars %= M.union argMap
        vars %= M.union declMap
        mapM_ evalStatement body

        vars .= archive 

    vars .= uniqueUnion archive (uniqueMap qualifiedFuncs)


-- later when I have the builtins written in Jack
-- I can get rid of this below and just compile them along


builtins :: M.Map Name Type
builtins = M.fromList [
    ("Math.abs",        FunT StaticFun 1),
    ("Math.multiply",   FunT StaticFun 2),
    ("Math.divide",     FunT StaticFun 2),
    ("Math.min",        FunT StaticFun 2),
    ("Math.max",        FunT StaticFun 2),
    ("Math.sqrt",       FunT StaticFun 1),
                                      
    ("String.new",            FunT Constructor 1),                     
    ("String.dispose",        FunT Method 0),                          
    ("String.length",         FunT Method 0),                          
    ("String.charAt",         FunT Method 1),                       
    ("String.setCharAt",      FunT Method 2),                          
    ("String.appendChar",     FunT Method 1),                          
    ("String.eraseLastChar",  FunT Method 0),                               
    ("String.intValue",       FunT Method 0),                          
    ("String.setInt",         FunT Method 1),     
    ("String.backSpace",      FunT StaticFun 0), 
    ("String.doubleQuote",    FunT StaticFun 0),                             
    ("String.newLine",        FunT StaticFun 0),

    ("Array.new", FunT Constructor 1),
    ("Array.dispose", FunT Method 0),

    ("Output.moveCursor"  , FunT StaticFun 2),                      
    ("Output.printChar"   , FunT StaticFun 1),                                                
    ("Output.printString" , FunT StaticFun 1),                     
    ("Output.printInt"    , FunT StaticFun 1),         
    ("Output.println"     , FunT StaticFun 0),      
    ("Output.backSpace"   , FunT StaticFun 0),

    ("Keyboard.keyPressed" , FunT StaticFun 0),                    
    ("Keyboard.readChar"   , FunT StaticFun 0),                 
    ("Keyboard.readLine"   , FunT StaticFun 1),                   
    ("Keyboard.readInt"    , FunT StaticFun 1),

    ("Memory.peek"   ,  FunT StaticFun 1),   
    ("Memory.poke"   ,  FunT StaticFun 2), 
    ("Memory.alloc"  ,  FunT StaticFun 1),
    ("Memory.deAlloc",  FunT StaticFun 1),

    ("Sys.halt"  , FunT StaticFun 0),                              
    ("Sys.error" , FunT StaticFun 1),                               
    ("Sys.wait"  , FunT StaticFun 1)]



initState :: CodeGenState
initState = (builtins, 0, 7, [])

runCodeGen :: (a -> CodeGen ()) -> a -> String 
runCodeGen gen a = unlines $ reverse $ (^.output) $ execState (gen a) initState                               
                                                             
compileClasses :: [Class] -> String 
compileClasses xs = let
    initState = (builtins, 0, 7, [])
    out = (^.output) $ execState (mapM_ evalClass xs) initState
    in unlines $ reverse out 
    