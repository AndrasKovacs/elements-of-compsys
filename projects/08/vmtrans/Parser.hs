{-# LANGUAGE ConstraintKinds #-}

module Parser (parseFile, parse) where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM 

import Data.Monoid
import Control.Applicative
import Data.Maybe

import Text.Trifecta hiding (Parser)
import ParserType (Parser(..))
import Text.Parser.Token.Highlight

import CodeGen


type Parsy m = (Monad m, TokenParsing m)

identStyle :: CharParsing m => IdentifierStyle m 
identStyle = IdentifierStyle {
    _styleName      = "identifierStyle", 
    _styleStart     = letter <|> oneOf "_.$:",
    _styleLetter    = letter <|> digit <|> oneOf "_.$:",
    _styleReserved  = HS.fromList [
        "push", "pop", "return", "function", 
        "goto", "if-goto", "label", "call",
        "add", "sub", "neg", "and", "or", "not", "eq", "lt", "gt",
        "static", "constant", "temp", "pointer", "argument", "local", "this", "that"],
    _styleHighlight         = Identifier,
    _styleReservedHighlight = ReservedIdentifier}

pLineOf :: Parsy m => m a -> m (Maybe a)
pLineOf p = whiteSpace *> ((Nothing <$ newline) <|> (Just <$> (p <* newline)))

pKeyword :: Parsy m => String -> m String
pKeyword kw = (kw <$ reserve identStyle kw) 

pLabelSymbol :: Parsy m => m String
pLabelSymbol = ident identStyle 

pLabel, pGoto, pGotoIf :: Parsy m => String -> m (Gen String)
pLabel  fn  = label   . ((fn++"$")++) <$> (pKeyword "label"   *> pLabelSymbol) 
pGoto   fn  = goto    . ((fn++"$")++) <$> (pKeyword "goto"    *> pLabelSymbol)
pGotoIf fn  = goto_if . ((fn++"$")++) <$> (pKeyword "if-goto" *> pLabelSymbol)

pReturn :: Parsy m => m (Gen String)
pReturn = goto "__return" <$ pKeyword "return"

pCall :: Parsy m => m (Gen String)
pCall = do 
    pKeyword "call"
    f <- pLabelSymbol
    n <- show <$> natural
    pure $ call_2 n f "__call_function" 

pPrimOp :: Parsy m => m (Gen String)
pPrimOp = call_0 . ("__"++) <$> choice (map pKeyword primops)
    where primops = ["add", "sub", "eq", "gt", "lt", "and", "neg", "not", "or"]

segmentBases :: HM.HashMap String Int
segmentBases = HM.fromList [
    ("static",      16),
    ("temp",        5),
    ("pointer",     3),
    ("argument",    2),
    ("local",       1),
    ("this",        3),
    ("that",        4)]

pStackOp :: Parsy m => m (Gen String)
pStackOp = do
    op  <- choice $ map pKeyword ["push", "pop"]
    seg <- choice $ map pKeyword ["pointer", "temp", "argument", "static", "constant", "this", "that", "local"]
    offset <- fromIntegral <$> integer

    let res | elem seg ["pointer", "temp", "static"] = 
                call_1 (show $ offset + segmentBases HM.! seg) ("__" ++ op ++ "_static")
            | seg == "constant" = 
                push_const (show offset)
            | otherwise = 
                call_2 (show $ segmentBases HM.! seg) (show offset) ("__" ++ op ++ "_dynamic")
    pure res 

pIns :: Parsy m => String -> m (Gen String)
pIns fn = choice [
    try pStackOp,
    pPrimOp,
    try $ pGoto fn,
    pGotoIf fn,
    pCall,
    pReturn,
    pLabel fn]

pFunction :: Parsy m => m (Gen String)
pFunction = do 
    fnLine <- pLineOf $ do 
        pKeyword "function"
        (,) <$> pLabelSymbol <*> (fromIntegral <$> natural)
    maybe (pure $ pure "")
        (\(fn, n) -> do 
            body <- catMaybes <$> (many $ pLineOf (pIns fn))
            pure $ catGens $ function_label fn n : body)
        fnLine

parser :: Parsy m => Parser m (Gen String)
parser = (catGens <$> many pFunction) <* eof

parseFile :: String -> IO (Gen String)
parseFile file = do
    inp <- (++"\n") <$> readFile file 
    case parseString (runParser parser) mempty inp of
        Success xs -> pure xs
        Failure e  -> error $ file ++ ": " ++ show e

parse :: String -> Gen String
parse s = 
    case parseString (runParser parser) mempty (s ++ "\n") of
        Success xs -> xs
        Failure e  -> error $ show e
