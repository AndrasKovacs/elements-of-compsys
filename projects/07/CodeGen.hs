{-# LANGUAGE FlexibleInstances #-}

module CodeGen (
    Gen, catGens,
    __eq, __gt, __lt,
    __add, __sub, __or,
    __and, __neg, __not,
    __push_dynamic, __push_static,
    __pop_static, __pop_dynamic,
    call_primop, call_static, call_dynamic,
    push_const, 
    goto, goto_if, label,
    init_sp, init_entry, init_fn, init_all) where


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
catGens = (unlines <$>) . sequence


-- ******************** initialization ***********************

endOfInit  = "__end_of_init"

init_sp :: Gen String
init_sp = pure $ unlines [
    "//  SP = 256",
    "@256",
    "D=A",
    "@SP",
    "M=D"]

init_entry :: String -> Gen String
init_entry entry = pure $ unlines [
    "//  goto entry point",
    printf "@%s" entry, -- entry
    "0;JMP"]

init_fn :: Gen String
init_fn = catGens [
    __eq, __gt, __lt,
    __add, __sub, __or,
    __and, __neg, __not,
    __push_dynamic, __push_static,
    __pop_static, __pop_dynamic]

init_all :: Gen String
init_all = catGens [
    init_sp, 
    init_entry endOfInit, 
    init_fn, 
    pure $ printf "(%s)" endOfInit]



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



-- ******************** inline functions *********************

-- binops, cmp, unops
call_primop :: String -> Gen String
call_primop fun = do
    ret <- newLabel
    pure $ unlines [
        printf "@%s"    ret,
               "D=A", 
        printf "@%d"    retReg,
               "M=D", 
        printf "@%s"    fun,  
               "0;JMP",
        printf "(%s)"   ret]

-- push/pop static, pointer, temp
call_static :: String -> String -> Gen String
call_static ref fun = do
    ret <- newLabel
    pure $ unlines [
        printf "@%s"    ret,
               "D=A", 
        printf "@%d"    retReg,
               "M=D",
        printf "@%s"    ref,
               "D=A", 
        printf "@%d"    argReg,
               "M=D", 
        printf "@%s"    fun,  
               "0;JMP",
        printf "(%s)"   ret]

-- push/pop argument, local, this, that
call_dynamic :: String -> String -> String -> Gen String
call_dynamic ref offset fun = do
    ret <- newLabel
    pure $ unlines [
        printf "@%s"    ret,
               "D=A", 
        printf "@%d"    retReg,
               "M=D",
        printf "@%s"    ref,
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

goto :: String -> Gen String
goto dest = pure $ unlines [
    printf "@%s" dest,
    "0;JMP"]

goto_if :: String -> Gen String
goto_if dest = pure $ unlines [
    "@SP",
    "A=M",
    "D=M",
    printf "@%s" dest,
    "D;JEQ"]



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
