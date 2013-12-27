
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
delimiter = void T.newline <|> void (T.char ';') <|> void T.eof 

spaces :: Parser String
spaces = T.many $ T.char ' '

chooseStr :: [String] -> Parser String
chooseStr xs = T.choice (map T.string xs) <* spaces

pComment :: Parser String
pComment = T.string "//" *> T.manyTill T.anyChar (lookAhead T.newline) 

pPrimOp :: Parser (Gen String)
pPrimOp = do
    op <- chooseStr ["add", "sub", "eq", "gt", "lt", "and", "neg", "not", "or"]
    pure $ Gen.call_primop $ "__" ++ op

pStackOp :: Parser (Gen String)
pStackOp = do
    op  <- chooseStr ["push", "pop"]
    seg <- chooseStr ["pointer", "temp", "argument", "static", "constant", "this", "that", "local"]
    offset <- T.integer' <* spaces 

    let res | elem seg ["pointer", "temp", "static"] = 
                Gen.call_static (show $ offset + segmentBases Map.! seg) (printf "__%s_static" op)
            | seg == "constant" = 
                Gen.push_const (show offset)
            | otherwise = 
                Gen.call_dynamic (show $ segmentBases Map.! seg) (show offset) (printf "__%s_dynamic" op)
    pure res 

pSymbol :: Parser String
pSymbol = (:) <$> nonDigit <*> T.many (nonDigit <|> T.digit) where
    nonDigit = T.letter <|> T.oneOf "_.$:"

pLabel :: Parser (Gen String)
pLabel = Gen.label <$> (T.string "label" *> spaces *> pSymbol <* spaces) 
    
pGoto :: Parser (Gen String)
pGoto = Gen.goto <$> (T.string "goto" *> spaces *> pSymbol <* spaces) 

pGotoIf :: Parser (Gen String)
pGotoIf = Gen.goto_if <$> (T.string "goto-if" *> spaces *> pSymbol <* spaces) 

pLine :: Parser (Gen String)
pLine = 
    spaces *>
    T.choice [
        pure "" <$ pComment,
        pure "" <$ lookAhead T.newline,
        T.try pStackOp,
        pPrimOp,
        T.try pGoto,
        pGotoIf,
        pLabel]
    <* (void pComment <|> delimiter)

parser :: Parser (Gen String)
parser = Gen.catGens <$> T.many pLine <* T.eof

parse :: String -> Gen String
parse s = 
    case T.parseString parser mempty s of
        T.Failure e   -> error (show e)
        T.Success gen -> gen 