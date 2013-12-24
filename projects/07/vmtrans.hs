{-# LANGUAGE LambdaCase #-}

import Text.Printf
import Control.Monad.State.Strict

-- MEM[5] : call_builtin return address
-- MEM[6] : pop/push ptr argument


init_sp :: String
init_sp = unlines [
    "@256",
    "D=A",
    "@SP",
    "M=D"]

call_builtin_0 :: String -> String -> String
call_builtin_0 = printf $ unlines [
    "@5",
    "M=%s", -- ret 
    "@%s",  -- fun
    "0;JMP"]

call_builtin_1 :: String -> String -> String -> String
call_builtin_1 = printf $ unlines [
    "@5",
    "M=%s", -- ret
    "@6",
    "M=%s", -- arg
    "@%s",  -- fun
    "0;JMP"]

__cmp :: String -> String -> String -> String
__cmp = printf $ unlines [
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@SP",
    "M=M-1",
    "A=M",
    "M=M-D",
    "@%s",   --true
    "M;J%s", --op
    "@SP",
    "A=M",
    "M=0",
    "@%s",   --end
    "0;JMP",
    "(%s)",  --true
    "@SP",
    "A=M",
    "M=1",
    "(%s)",  --end
    "@SP",
    "M=M+1",
    "@5",
    "A=M",
    "0;JMP"]

__binop :: String -> String -> String
__binop = printf $ unlines [
    "(%s)",  --addr
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@SP",
    "M=M-1",
    "A=M",
    "M=M%sD", --op
    "@SP",
    "M=M+1",
    "@5",
    "A=M",
    "0;JMP"]

__unop :: String -> String -> String 
__unop = printf $ unlines [
    "(%s)",  -- addr 
    "@SP",
    "M=M-1",
    "A=M",
    "M=%sM", -- op
    "@SP",
    "M=M+1",
    "@5",
    "A=M",
    "0;JMP"]

__push_from_ref :: String -> String
__push_from_ref = printf $ unlines [
    "(%s)",   -- addr 
    "@6",
    "A=M",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",
    "@5",
    "A=M",
    "0;JMP"] 

__push_from_seg :: String -> String
__push_from_seg = printf $ unlines [
    "(%s)",   -- addr 
    "@6",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1",
    "@5",
    "A=M",
    "0;JMP"] 

__pop_to_ref :: String -> String
__pop_to_ref = printf $ unlines [
    "(%s)",   -- addr 
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@6",
    "A=M",
    "M=D",
    "@5",
    "A=M",
    "0;JMP"] 

__pop_to_seg :: String -> String
__pop_to_seg = printf $ unlines [
    "(%s)",   -- addr 
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@6",
    "M=D",
    "@5",
    "A=M",
    "0;JMP"] 



-- INLINE
__push_const :: String -> String
__push_const = printf $ unlines [
    "@%s", --const
    "D=A",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"]






    