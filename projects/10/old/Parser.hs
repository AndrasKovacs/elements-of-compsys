{-# LANGUAGE 
    ConstraintKinds, TupleSections, NoMonomorphismRestriction, 
    FlexibleContexts, LambdaCase, PatternGuards #-}

import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HM 

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Int

import Text.Trifecta hiding (Parser)
import Text.Parser.Token.Highlight
import Text.Parser.Expression

import Types 


-- ************************************************

type Parsy  m = (Monad m, TokenParsing m)
type Staty m = MonadState ParseState m 

vars :: Lens' ParseState (HM.HashMap String Type)
vars = _1 

objTypes :: Lens' ParseState (HM.HashMap String (HM.HashMap String Type))
objTypes = _2 


-- **********************************************

--builtinTypes = HM.fromList [
--    ("int", 



-- ****************** identstyle *******************


identStyle :: CharParsing m => IdentifierStyle m 
identStyle = IdentifierStyle {
    _styleName      = "identifierStyle", 
    _styleStart     = letter <|> char '_',
    _styleLetter    = letter <|> digit <|> char '_',
    _styleReserved  = HS.fromList [
        "class", "constructor", "method", "function",
        "int", "boolean", "char", "void",
        "var", "static", "field",
        "let", "do", "if", "else", "while", "return",
        "true", "false", "null",
        "this"],
    _styleHighlight         = Identifier,
    _styleReservedHighlight = ReservedIdentifier}

pIdentifier :: Parsy m => m String
pIdentifier = ident identStyle

pVariable :: (Parsy m, Staty m) => m (String, Type)
pVariable = do
    ident <- pIdentifier
    use (vars . at ident) >>= \case
        Just t -> pure (ident, t)
        _      -> fail "identifier not found in scope"

pReserved :: Parsy m => String -> m ()
pReserved = reserve identStyle



-- ****************** expressions *******************


pExpr = undefined

pLit :: Parsy m => m (Expr, Type)
pLit = pInteger <|> pBool <|> pString where
    pString  = do
        e <- char '"' *> (Lit . S <$> many (noneOf "\"\n")) <* char '"'
        pure (e, DataT (ObjectT Rvalue "Array"))
    pInteger = do
        e <- Lit . I . fromIntegral <$> natural
        pure (e, DataT (PrimT Rvalue IntT))
    pBool = do
        e <- (Lit (B True)  <$ pReserved "true") <|> (Lit (B False) <$ pReserved "false")
        pure (e, DataT (PrimT Rvalue BoolT))

pIndex :: (Parsy m, Staty m) => m (
pIndex = do
    (arr, t) <- pVariable
    case t of
        DataT (ObjectT _ "Array") -> (

--pCall :: Parsy m => m Expr 
--pCall = Call <$> pVariable <*> 

--pAdd = binary '+' Add AssocLeft
--pSub 



--binary :: Parsy m => Char -> (a -> a -> a) ->  m (a -> a -> a)
--binary  name f  = Infix   (f <$ symbolic name) 

--binary :: Parsy m => Char -> (a -> a -> a) -> Assoc -> Operator m a
--binary name f = Infix (f <$ symbolic name) 

--prefix :: Parsy m => Char -> (a -> a) -> Operator m a
--prefix name f = Prefix (f <$ symbolic name)



--term = parens expr <|> pString <|> 

--table = [
--    [prefix '-' Neg, prefix '!' Not],
--    [

--data Expr      = Add Expr Expr    
--               | Sub Expr Expr    
--               | Neg Expr       
--               | Not Expr       
--               | Mul Expr Expr   
--               | Div Expr Expr    
--               | And Expr Expr  
--               | Or Expr Expr   
--               | Cmp Ordering Expr Expr
--               | Lit ALit 
--               | Index Name Expr
--               | Call Name [Expr]
--               deriving (Eq, Show)


--data ALit      = I Int16
--               | S String 
--               | C Char 
--               | B Bool 
--               deriving (Eq, Show)



--main = parseTest expr "12312 + 3423 - 123 - 32"

--expr = buildExpressionParser table term


--term =  natural


--table = [ [prefix "-" negate, prefix "+" id ]
--        , [postfix "++" (+1)]
--        , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
--        , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
--        ]

--binary  name fun  = Infix   (fun <$ symbol name) 
--prefix  name fun  = Prefix  (fun <$ symbol name)
--postfix name fun  = Postfix (fun <$ symbol name)