
module Parser (parse) where 

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Monoid
import qualified Data.Map as Map 
import           Text.Printf

import           Text.Trifecta (Parser)
import qualified Text.Trifecta as T
import           Text.Parser.LookAhead (lookAhead)

import           CodeGen (Gen)
import qualified CodeGen as Gen  


segmentBases = Map.fromList [
    ("static",      16),
    ("temp",        5),
    ("pointer",     3),
    ("argument",    2),
    ("local",       1),
    ("this",        3),
    ("that",        4)]

delimiter :: Parser () 
delimiter = void T.newline <|> void (T.char ';') <|> lookAhead (void T.eof)

spaces :: Parser String
spaces = T.many $ T.char ' '

chooseStr :: [String] -> Parser String
chooseStr xs = T.choice (map T.string xs) <* spaces

pComment :: Parser String
pComment = T.string "//" *> T.manyTill T.anyChar (lookAhead (void T.newline <|> void T.eof)) 

pPrimOp :: Parser (Gen String)
pPrimOp = do
    op <- chooseStr ["add", "sub", "eq", "gt", "lt", "and", "neg", "not", "or"]
    pure $ Gen.call_0 $ "__" ++ op

pStackOp :: Parser (Gen String)
pStackOp = do
    op  <- chooseStr ["push", "pop"]
    seg <- chooseStr ["pointer", "temp", "argument", "static", "constant", "this", "that", "local"]
    offset <- T.integer' <* spaces 

    let res | elem seg ["pointer", "temp", "static"] = 
                Gen.call_1 (show $ offset + segmentBases Map.! seg) (printf "__%s_static" op)
            | seg == "constant" = 
                Gen.push_const (show offset)
            | otherwise = 
                Gen.call_2 (show $ segmentBases Map.! seg) (show offset) (printf "__%s_dynamic" op)
    pure res 

pSymbol :: Parser String
pSymbol = (:) <$> nonDigit <*> T.many (nonDigit <|> T.digit) where
    nonDigit = T.letter <|> T.oneOf "_.$:"

pLabel :: String -> Parser (Gen String)
pLabel funcName = do
    sym <- T.string "label" *> spaces *> pSymbol <* spaces 
    pure $ Gen.label $ funcName ++ "$" ++ sym
    
pGoto :: Parser (Gen String)
pGoto = Gen.goto <$> (T.string "goto" *> spaces *> pSymbol <* spaces) 

pGotoIf :: Parser (Gen String)
pGotoIf = Gen.goto_if <$> (T.string "goto-if" *> spaces *> pSymbol <* spaces) 

pReturn :: Parser (Gen String)
pReturn = Gen.goto "__return" <$ T.string "return" <* spaces 

pCall :: Parser (Gen String)
pCall = do
    T.string "call" <* spaces
    f <- pSymbol <* spaces
    n <- T.some T.digit <* spaces
    pure $ Gen.call_2 (show n) f "__call_function"

pFunction :: Parser (Gen String)
pFunction = do
    spaces *> T.string "function" <* spaces
    fname <- pSymbol <* spaces
    localSpace <- T.some T.digit <* spaces <* delimiter
    let label = printf "(%s)" fname
    body <- T.many (T.try $ pLine fname)
    pure $ Gen.catGens $ pure label : body

pLine :: String -> Parser (Gen String)
pLine funcName = 
    spaces *>
    T.choice [
        pure "" <$ pComment,
        pure "" <$ lookAhead T.newline,
        T.try pStackOp,
        pPrimOp,
        T.try pGoto,
        pGotoIf,
        pCall,
        pReturn,
        pLabel funcName]
    <* (void pComment <|> delimiter)

parser :: Parser (Gen String)
parser = (Gen.catGens <$> T.many pFunction) <* T.eof

parse :: String -> Gen String
parse s = 
    case T.parseString parser mempty s of
        T.Failure e   -> error (show e)
        T.Success gen -> gen 
