{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.FilePath
import Control.Monad.State.Strict 

import CodeGen (catGens, init_all)
import Parser (parse)

compile :: [String] -> String
compile xs = evalState (catGens $ init_all : map parse xs) 0

main :: IO ()
main = 
    getArgs >>= \case
        out:ins@(:){} -> writeFile out . compile =<< mapM readFile ins
        _             -> putStrLn "usage: vmtrans [output] [inputs...]"
