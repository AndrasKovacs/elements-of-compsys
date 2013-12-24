{-# LANGUAGE LambdaCase #-}


import qualified Text.Trifecta as T
import           Text.Trifecta (Parser)
import           Text.Parser.LookAhead (lookAhead) 

import Control.Applicative
import Control.Monad
import Data.Monoid
import Text.Printf
import System.Environment
import Data.List

import qualified IR 


initSP = IR.compile "SP = 256"


static    = "16"
stackBase = "256"
heapBase  = "2048"
tempBase  = "5"


binop = IR.compile . printf "SP = *SP - 1; *SP - 1 = *(*SP - 1) %s **SP" 
unop  = IR.compile . printf "SP = *SP - 1; *SP = %s*SP; SP = *SP + 1" 


pushFromRef src i = IR.compile $ printf "*SP = *(*%s + %d); SP = *SP + 1" src i 
pushFromSeg src i = IR.compile $ printf "*SP = *( %s + %d); SP = *SP + 1" src i
pushConstant i    = IR.compile $ printf "*SP = %d; SP = *SP + 1" i


popToRef src i = IR.compile $ printf "SP = *SP - 1; *%s + %d = **SP" src i 
popToSeg src i = IR.compile $ printf "SP = *SP - 1;  %s + %d = **SP" src i 
popConstant _  = error ""


spOp constant ref seg  = \case
    "argument"-> ref "ARG"    
    "local"   -> ref "LCL"    
    "static"  -> seg static   
    "constant"-> constant     
    "this"    -> ref "THIS"   
    "that"    -> ref "THAT"   
    "pointer" -> seg "THIS"   
    "temp"    -> seg tempBase 
    _         -> const (error "")


spaces :: Parser [Char]
spaces = T.many $ T.char ' '

token :: Parser String
token = T.some T.letter <* spaces

pOp :: Parser String
pOp = do
    op <- token
    pure $ case op of 
        "add" -> binop "+"
        "sub" -> binop "-"
        "eq"  -> binop "=="
        "gt"  -> binop ">"
        "lt"  -> binop "<"
        "and" -> binop "&"
        "neg" -> unop "-"
        "not" -> unop "!"
        _     -> fail ""

pSpOp :: Parser String
pSpOp = do
    op  <- token 
    seg <- token 
    i   <- T.integer' <* spaces
    let f = case op of 
            "push" -> spOp pushConstant pushFromRef pushFromSeg  
            "pop"  -> spOp popConstant  popToRef    popToSeg 
            _      -> fail ""
    pure $ f seg i

pComment :: Parser ()
pComment = void $ T.string "//" *> T.manyTill T.anyChar (lookAhead T.newline)

pLine :: Parser String 
pLine = 
    spaces *>
    (("" <$ pComment) <|> lookAhead ("" <$ T.newline) <|> T.try pSpOp <|> pOp)
    <* (pComment <|> void T.newline <|> void (T.char ';'))

compile :: String -> String 
compile s = 
    case T.parseString (unlines . filter (not.null) <$> T.many pLine) mempty (s ++ "\n") of
        T.Failure e   -> error (show e)
        T.Success out -> out


tst = unlines [
    "push constant 10",
    "push constant 10"]

main :: IO ()
main = do
    putStrLn $ compile "add"
    getArgs >>= \case
        [inp, out] -> writeFile out =<< compile <$> readFile inp
        _          -> putStrLn "usage: vmtrans [input] [output]"

