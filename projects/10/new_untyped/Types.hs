
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (

      Name
    , Type(..)

    -- Parser
    , Parser(..)

    -- AST
    , Expr(..)
    , Statement(..)
    , ALit(..)
    , Function(..)
    , Class(..)
    , VarSite(..)
    , FunSite(..)

    ) where

import Control.Applicative
import Control.Monad
import Data.Int
import Control.DeepSeq

import Text.Trifecta (CharParsing, Parsing, Errable, DeltaParsing, TokenParsing, someSpace, space)
import Text.Parser.Token.Style (CommentStyle(..), buildSomeSpaceParser, javaCommentStyle)
import Text.Parser.LookAhead (LookAheadParsing)

-- types

type Name = String

data Type = VarT VarSite Name Int -- site, offset
          | FunT FunSite Int -- site, arity 
          deriving (Eq, Show)

instance NFData Type where
    rnf (VarT a b c) = a `seq` b `seq` c `seq` ()
    rnf (FunT a b)   = a `seq` b `seq` ()

-- parser

newtype Parser m a = Parser {runParser :: m a} deriving (
    Functor, Monad, Applicative, Alternative, MonadPlus,
    CharParsing, LookAheadParsing, Parsing, DeltaParsing)

instance (CharParsing m, MonadPlus m) => TokenParsing (Parser m) where
    someSpace = buildSomeSpaceParser (() <$ space) javaCommentStyle

-- AST

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
               | MethodCall Name Expr -- foo.bar(a,b) where bar(a,b) is the expr
               | Var Name 
               deriving (Eq, Show)

data Statement = LetIndex Expr Expr
               | Let Name Expr
               | If Expr [Statement] (Maybe [Statement])
               | While Expr [Statement]
               | Do Expr
               | Return Expr
               deriving (Eq, Show)

data ALit      = I Int16
               | S String 
               | C Char 
               | B Bool 
               deriving (Eq, Show)

data VarSite = StaticVar | Field | Local | Argument deriving (Eq, Show)
data FunSite = StaticFun | Method | Constructor deriving (Eq, Show)

data Class    = Class Name [(VarSite, Name, [Name])] [Function] deriving (Eq, Show)
data Function = Function FunSite Name [(Name, Name)] [(Name, [Name])] [Statement] deriving (Eq, Show)