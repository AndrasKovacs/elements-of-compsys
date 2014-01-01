
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Types (

    -- Types
      Name
    , APrimT(..)
    , DataSite(..)
    , FuncSite(..)
    , ADataT(..)
    , Type(..)

    -- Parser
    , Parser(..)
    , ParseState
    , runParser
    , runParserWith

    -- AST
    , Expr(..)
    , Statement(..)
    , VarDecl(..)
    , ALit(..)
    , Function(..)
    , Class(..)

    ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Data.HashMap.Strict
import Data.Monoid
import Data.Int

import Text.Trifecta (CharParsing, Parsing, Errable, DeltaParsing, TokenParsing, someSpace, space)
import Text.Parser.Token.Style (CommentStyle(..), buildSomeSpaceParser, javaCommentStyle)
import Text.Parser.LookAhead (LookAheadParsing)


-- ********************************** Types ***************************************

type Name = String

-- only top level Objects should have Top DataSite. 
-- VarDecl decl should have only static or field datasite. 

--data APrimT = BoolT | IntT | CharT | Void deriving (Eq, Show)
--data DataSite = StaticDat | Field | Local | Rvalue | Top deriving (Eq, Show)
--data FuncSite = StaticFun | Method | Constructor deriving (Eq, Show)
--data ADataT = ObjectT DataSite Name | PrimT DataSite APrimT deriving (Eq, Show)
--data Type = Unspecified | DataT ADataT | FunctionT FuncSite [ADataT] ADataT deriving (Eq, Show)


-- ******************** parser with comments and state ******************************

type ParseState = (
    HashMap String Type,                 -- names and types of current scope
    HashMap String (HashMap String Type) -- names and member type dictionaries of current object types
    )

newtype Parser m a = Parser (StateT ParseState m a) deriving (
    Functor, Monad, Applicative, Alternative, MonadPlus,
    CharParsing, LookAheadParsing, Parsing, DeltaParsing)

instance (CharParsing m, MonadPlus m) => TokenParsing (Parser m) where
    someSpace = buildSomeSpaceParser (() <$ space) javaCommentStyle

instance MonadTrans Parser where
    lift = Parser . lift 

runParser :: Monad m => Parser m a -> m a
runParser = runParserWith mempty

runParserWith :: Monad m => ParseState -> Parser m a -> m a
runParserWith state (Parser sma) = evalStateT sma state



-- ***************************** AST **************************************

data Expr      = Add Expr Expr    
               | Sub Expr Expr    
               | Neg Expr       
               | Not Expr       
               | Mul Expr Expr   
               | Div Expr Expr    
               | And Expr Expr  
               | Or Expr Expr   
               | Cmp Ordering Expr Expr
               | Lit ALit 
               | Index Name Expr
               | Call Name [Expr]
               | Var Name 
               deriving (Eq, Show)

data Statement = LetIndex Name Expr Expr
               | Let Name Expr
               | If [(Expr, [Statement])]
               | While Expr [Statement]
               | Do Name [Expr]
               | Return (Maybe Expr)
               deriving (Eq, Show)

data ALit      = I Int16
               | S String 
               | C Char 
               | B Bool 
               deriving (Eq, Show)

data Class    = Class Name [VarDecl] [Function]
data VarDecl      = VarDecl ADataT [Name]
data Function = Function FuncSite ADataT [(ADataT, Name)] [Statement]