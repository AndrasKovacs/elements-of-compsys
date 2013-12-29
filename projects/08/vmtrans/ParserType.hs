
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Transformer for Trifecta with simple line comments and whitespace parsing without newlines.

module ParserType (Parser(..)) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Text.Trifecta (CharParsing, Parsing, Errable, DeltaParsing, TokenParsing, someSpace, char)
import Text.Parser.Token.Style (CommentStyle(..), buildSomeSpaceParser)
import Text.Parser.LookAhead (LookAheadParsing)

newtype Parser m a = Parser {runParser :: m a} deriving (
    Functor, Monad, Applicative, Alternative, MonadPlus,
    CharParsing, LookAheadParsing, Parsing, Errable, DeltaParsing)

instance TokenParsing m => TokenParsing (Parser m) where
    someSpace = buildSomeSpaceParser (() <$ char ' ') (CommentStyle "" "" "//" False)

instance MonadTrans Parser where
    lift = Parser
