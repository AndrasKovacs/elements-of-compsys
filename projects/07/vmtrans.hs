{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.FilePath
import Control.Monad.State.Strict 

import CodeGen (catGens, init_all)
import Parser (parse)

compile :: String -> String
compile s = evalState (catGens [init_all, parse s]) 0

--main :: IO ()
--main = 
--    getArgs >>= \case
--        [inp, out] -> writeFile out . compile =<< readFile inp
--        _          -> putStrLn "usage: vmtrans [input] [output]"

main = do
    --putStrLn $ compile "function add 0; push constant 3; push constant 5; add; return"
    putStrLn $ compile $ unlines [
        "function Sys.init 0",
            "push constant 4",
            "call fibonacci 1",

        "function fibonacci 0",
            "push argument 0",
            "push constant 2",
            "lt",
            "goto-if end",
                "push argument 0",
                "push constant 1",
                "sub",
                "call fibonacci 1",
                "push argument 0",
                "push constant 2",
                "sub",
                "call fibonacci 1",
                "add",
                "return",
            "label end",
                "push argument 0",
                "return"]

