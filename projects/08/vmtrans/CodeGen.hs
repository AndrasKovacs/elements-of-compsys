
module CodeGen (
    Gen, catGens, evalGen,
    __eq, __gt, __lt,
    __add, __sub, __or,
    __and, __neg, __not,
    __push_dynamic, __push_static,
    __pop_static, __pop_dynamic,
    __call_function, __return,
    call_0, call_1, call_2,
    push_const, 
    goto, goto_if, function_label, label, 
    init_all) where


import Text.Printf
import Control.Monad.State.Strict
import Control.Applicative


{- builtin/primops calling convention:
    - compsys book page 222 says memory 13-16 are general purpose VM registers
    MEM[13] : return address
    MEM[14] : pop/push ref argument
    MEM[15] : pop/push offset argument
-}

{- NOTE : -1 is true, 0 is false ! -}

retReg    = 13 :: Int 
argReg    = 14 :: Int 
offsetReg = 15 :: Int


-- ******************** label state **************************

type Gen = State Int 

newLabel :: Gen String
newLabel = (printf "__%d" <$> get) <* modify (+1)

catGens :: [Gen String] -> Gen String
catGens = (unlines . filter (not . null) <$>) . sequence

evalGen :: Gen a -> a
evalGen = (`evalState` 0)


-- ******************** initialization ***********************

entry  = "Sys.init"

init_sp :: Gen String
init_sp = pure $ unlines [
    "//  SP = 256",
    "@256",
    "D=A",
    "@SP",
    "M=D"]

init_fn :: Gen String
init_fn = catGens [
    __eq, __gt, __lt,
    __add, __sub, __or,
    __and, __neg, __not,
    __push_dynamic, __push_static,
    __pop_static, __pop_dynamic,
    __call_function, __return]

init_all :: Gen String
init_all = catGens [
    init_sp, 
    goto entry, 
    init_fn,
    pure "//****************** start of text ******************"]



-- ******************** builtin functions *********************

__eq = cmp "__eq" "EQ"
__gt = cmp "__gt" "GT"
__lt = cmp "__lt" "LT"

__add = binop "__add" "+"
__sub = binop "__sub" "-"
__or  = binop "__or"  "|"
__and = binop "__and" "&"
__neg = unop  "__neg" "-"
__not = unop  "__not" "!"

__push_dynamic :: Gen String
__push_dynamic = pure $ unlines [
    "(__push_dynamic)",
    printf "@%d" offsetReg,
    "D=M",
    printf "@%d" argReg,
    "A=M",
    "A=D+M",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",
    printf "@%d" retReg,
    "A=M",
    "0;JMP"]

__push_static :: Gen String
__push_static = pure $ unlines [
    "(__push_static)", 
    printf "@%d" argReg,
    "A=M",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",
    printf "@%d" retReg,
    "A=M",
    "0;JMP"] 

__pop_dynamic :: Gen String
__pop_dynamic = pure $ unlines [
    "(__pop_dynamic)",
    printf "@%d" offsetReg,
    "D=M",
    printf "@%d" argReg,
    "A=M",
    "D=D+M",
    printf "@%d" argReg,
    "M=D",
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    printf "@%d" argReg,
    "A=M",
    "M=D",
    printf "@%d" retReg,
    "A=M",
    "0;JMP"]  

__pop_static :: Gen String
__pop_static = pure $ unlines [
    "(__pop_static)",
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    printf "@%d" argReg,
    "A=M",
    "M=D",
    printf "@%d" retReg,
    "A=M",
    "0;JMP"] 

__call_function :: Gen String
__call_function = pure $ unlines [ 
    "(__call_function)",
    printf "@%d" retReg,  -- ret addr
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",

    "@LCL",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",

    "@ARG",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",

    "@THIS",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",

    "@THAT",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",

    "@SP",
    "D=M",
    "@5",
    "D=D-A",
    printf "@%d" argReg, -- number of args
    "D=D-M",
    "@ARG",
    "M=D",

    "@SP",
    "D=M",
    "@LCL",
    "M=D",

    printf "@%d" offsetReg, -- func address
    "A=M",
    "0;JMP"]

__return :: Gen String  -- PRIMOPS
__return = pure $ unlines [
    "(__return)",
    "@5", -- 14 = *(LCL - 5)
    "D=A",
    "@LCL",
    "A=M-D",
    "D=M",
    "@14",
    "M=D",

    "@SP", -- *ARG = *(SP - 1)
    "A=M-1",
    "D=M",
    "@ARG",
    "A=M",
    "M=D",

    "@ARG",  -- SP = ARG + 1
    "D=M+1",
    "@SP",
    "M=D",

    "@LCL", -- 13 = LCL
    "D=M",
    "@13",

    "M=D-1", -- pop that
    "A=M", 
    "D=M",
    "@THAT",
    "M=D",

    "@13", -- pop this
    "M=M-1",
    "A=M", 
    "D=M",
    "@THIS",
    "M=D",

    "@13", -- pop arg
    "M=M-1",
    "A=M", 
    "D=M",
    "@ARG",
    "M=D",

    "@13", --  pop lcl
    "M=M-1",
    "A=M", 
    "D=M",
    "@LCL",
    "M=D",

    "@14", -- return
    "A=M", 
    "0;JMP"]



-- ******************** inline functions *********************

-- binops, cmp, unops
call_0 :: String -> Gen String
call_0 fun = do
    ret <- ("__RET"++) <$> newLabel
    pure $ unlines [
        printf "@%s"    ret,
               "D=A", 
        printf "@%d"    retReg,
               "M=D", 
        printf "@%s"    fun,  
               "0;JMP",
        printf "(%s)"   ret]

-- push/pop static, pointer, temp
call_1 :: String -> String -> Gen String
call_1 arg fun = do
    ret <- ("__RET"++) <$> newLabel
    pure $ unlines [
        printf "@%s"    ret,
               "D=A", 
        printf "@%d"    retReg,
               "M=D",
        printf "@%s"    arg,
               "D=A", 
        printf "@%d"    argReg,
               "M=D", 
        printf "@%s"    fun,  
               "0;JMP",
        printf "(%s)"   ret]

-- push/pop argument, local, this, that
call_2 :: String -> String -> String -> Gen String
call_2 arg offset fun = do
    ret <- ("__RET"++) <$> newLabel
    pure $ unlines [
        printf "@%s"    ret,
               "D=A", 
        printf "@%d"    retReg,
               "M=D",
        printf "@%s"    arg,
               "D=A",
        printf "@%d"    argReg,
               "M=D", 
        printf "@%s"    offset,
               "D=A", 
        printf "@%d"    offsetReg,
               "M=D",
        printf "@%s"    fun, 
               "0;JMP",
        printf "(%s)"   ret]

push_const :: String -> Gen String
push_const cons = pure $ unlines [
    printf "@%s" cons, --cons
    "D=A",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"]

label :: String -> Gen String
label = pure . printf "(%s)\n"

function_label :: String -> Int -> Gen String
function_label name n = catGens $ (label name) : replicate n (push_const "0")

goto :: String -> Gen String
goto dest = pure $ unlines [
    printf "@%s" dest,
    "0;JMP"]

goto_if :: String -> Gen String
goto_if dest = pure $ unlines [
    "@SP",
    "A=M-1",
    "D=M",
    printf "@%s" dest,
    "D;JNE"]


-- ******************** factory functions *********************

cmp :: String -> String -> Gen String
cmp name op = do
    true <- newLabel
    end  <- newLabel
    pure $ unlines [
        printf "(%s)" name,
        "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        "@SP",
        "M=M-1",
        "A=M",
        "D=M-D",
        printf "@%s" true, 
        printf "D;J%s" op,
        "@SP",
        "A=M",
        "M=0",
        printf "@%s" end,
        "0;JMP",
        printf "(%s)" true,
        "@SP",
        "A=M",
        "M=-1",
        printf "(%s)" end,
        "@SP",
        "M=M+1",
        printf "@%d" retReg,
        "A=M",
        "0;JMP"]

binop :: String -> String -> Gen String
binop name op = pure $ unlines [
    printf "(%s)" name,
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@SP",
    "M=M-1",
    "A=M",
    printf "M=M%sD" op,
    "@SP",
    "M=M+1",
    printf "@%d" retReg,
    "A=M",
    "0;JMP"]

unop :: String -> String -> Gen String 
unop name op = pure $ unlines [
    printf "(%s)" name,
    "@SP",
    "M=M-1",
    "A=M",
    printf "M=%sM" op,
    "@SP",
    "M=M+1",
    printf "@%d" retReg,
    "A=M",
    "0;JMP"]
