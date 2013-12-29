{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.FilePath

import CodeGen (Gen, catGens, init_all, evalGen)
import Parser (parseFile)

compile :: [Gen String] -> String
compile = evalGen . catGens . (init_all:)

main :: IO ()
main = 
    getArgs >>= \case
        out:ins@(:){} -> writeFile out . compile =<< mapM parseFile ins 
        _             -> putStrLn "usage: vmtrans [output] [inputs...]"
