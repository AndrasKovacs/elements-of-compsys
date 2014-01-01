{-# LANGUAGE LambdaCase, NoMonomorphismRestriction #-}

module Parser (parseFile, Parser.parseTest) where

import qualified Data.HashSet as Set
import Control.Applicative
import Data.Monoid

import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style

import Types



-- identifiers

identStyle = IdentifierStyle {
    _styleName      = "identifierStyle", 
    _styleStart     = letter <|> char '_',
    _styleLetter    = letter <|> digit <|> char '_',
    _styleReserved  = Set.fromList [
        "class", "constructor", "method", "function",
        "int", "boolean", "char", "void",
        "var", "static", "field",
        "let", "do", "if", "else", "while", "return",
        "true", "false", "null",
        "this"],
    _styleHighlight         = Identifier,
    _styleReservedHighlight = ReservedIdentifier}

pIdent = ident   identStyle <?> "identifier"
pRes   = reserve identStyle



-- expresssions

prefix       f op = Prefix (f <$ symbolic op)
binary assoc f op = Infix  (f <$ symbolic op) assoc

infixL = binary AssocLeft
infixR = binary AssocRight
infixN = binary AssocNone

opTable = [
    [prefix Neg '-', prefix Not '~'],
    [infixL Mul '*', infixL Div '/'],
    [infixL Add '+', infixL Sub '-'],
    [infixR (Cmp LT) '<', infixR (Cmp GT) '>', infixR (Cmp EQ) '='],
    [infixR And '&'],
    [infixR Or '|']]

pInt    = Lit . I . fromIntegral <$> natural <?> "integer"
pString = char '"' *> (Lit . S <$> many (noneOf "\"\n")) <* char '"' <* whiteSpace
pChar   = Lit . C <$> charLiteral
pBool   = (Lit (B False) <$ pRes "false") <|> (Lit (B True) <$ pRes "true")
pThis   = Var <$> ("this" <$ pRes "this")
pNull   = Lit (I 0) <$ pRes "null"

pLit    = pInt <|> pString <|> pChar <|> pBool <|> pThis <|> pNull

pIndex  = Index      <$> pIdent <*> brackets pExpr
pCall   = Call       <$> pIdent <*> parens (commaSep pExpr)
pMethod = MethodCall <$> pIdent <*> (dot *> pCall) 
pVar    = Var        <$> pIdent 

pVarOp  = try pIndex <|> try pCall <|> try pMethod <|> pVar -- NOTE: factor out pIdents
pTerm   = parens pExpr <|> pVarOp <|> pLit
pExpr   = buildExpressionParser opTable pTerm



-- statements
 
pLet      = Let      <$> (pRes "let"    *> pIdent) <*> (symbolic '=' *> pExpr <* semi)
pLetIndex = LetIndex <$> (pRes "let"    *> pIndex) <*> (symbolic '=' *> pExpr <* semi)
pIf       = If       <$> (pRes "if"     *> parens pExpr) <*> pBlock <*> optional (pRes "else" *> pBlock)
pWhile    = While    <$> (pRes "while"  *> parens pExpr) <*> pBlock
pDo       = Do       <$> (pRes "do"     *> (try pCall <|> pMethod)) <* semi
pReturn   = Return   <$> (pRes "return" *> option (Lit (I 0)) pExpr) <* semi

pStatement = pReturn <|> pDo <|> pWhile <|> pIf <|> try pLetIndex <|> pLet
pBlock     = braces (many pStatement)



-- functions & classes

pFunSite = choice $ zipWith (\a b -> a <$ pRes b)
    [Types.Constructor, Method, StaticFun]
    ["constructor", "method", "function"]

pClassVarSite = choice $ zipWith (\a b -> a <$ pRes b)
    [Field, StaticVar]
    ["field", "static"]

pType = (choice $ map symbol ["int", "char", "boolean", "void"]) <|> pIdent

pLocalVar = (,) <$> (pRes "var" *> pType) <*> commaSep pIdent <* semi
pClassVar = (,,) <$> pClassVarSite <*> pType <*> commaSep pIdent <* semi

pFunction = Function <$> 
        (pFunSite <* pType)
    <*> pIdent 
    <*> parens (commaSep ((,) <$> pType <*> pIdent)) 
    <*> (symbolic '{' *> many pLocalVar) 
    <*> (many pStatement <* symbolic '}')

pClass = Class <$>
        (pRes "class" *> pIdent)
    <*> (symbolic '{' *> many pClassVar)
    <*> (many pFunction <* symbolic '}')


-- program

pJack = whiteSpace *> pClass <* eof 

parseFile :: String -> IO Class
parseFile path = maybe (error "")
    pure =<< (parseFromFile (runParser pJack) path)

parseTest :: String -> IO ()
parseTest = Text.Trifecta.parseTest (runParser pJack)
