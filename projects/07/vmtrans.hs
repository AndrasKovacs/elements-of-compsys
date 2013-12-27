{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import System.Environment
import System.FilePath
import Control.Monad.State.Strict 

import CodeGen (catGens, init_all)
import Parser (parse)

compile :: String -> String
compile s = evalState (catGens [init_all, parse s]) 0

main :: IO ()
main = 
    getArgs >>= \case
        [inp, out] -> writeFile out . compile =<< readFile inp
        [inp]      -> writeFile (replaceExtension inp ".vm") . compile =<< readFile inp 
        _          -> putStrLn "usage: vmtrans [input] [output]"

