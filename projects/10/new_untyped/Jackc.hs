{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import System.Environment
import System.Directory
import System.FilePath

import Types
import Parser 
import CodeGen


{- TODO : 
    - throw error when the class name doesn't match the file name
    - method calls done right.
    - testing
    - some kind of principled way to do everything inside the parser, thus preserving the line information?
-}

main :: IO ()
main = 
    getArgs >>= \case
        [outPath, inPath] -> doesDirectoryExist inPath >>= \case
            True -> do 
                sources <- filter ((".jack"==) . takeExtension) <$> getDirectoryContents inPath 
                if null sources
                    then putStrLn "no source files in directory"
                    else writeFile outPath =<< compileClasses <$> mapM parseFile sources
            _ -> 
                if takeExtension inPath == ".jack"
                    then writeFile outPath =<< compileClasses . (:[]) <$> parseFile inPath
                    else putStrLn "wrong input file extension"
        _ -> putStrLn "usage: [output] [input file or directory]"



                        

    







