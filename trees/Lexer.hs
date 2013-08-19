module Lexer where

import CCO.Parsing (Parser(..), satisfy, Symbol(describe))
import CCO.Lexing (Lexer(..), digit_, anyCharFrom)
import Data.Char (isDigit)
import Control.Applicative

data Token
        = Integer {fromInt :: Int}
        | Terminal {fromTerm :: Char}

instance Symbol Token where
    describe (Integer _)    lex = "integer " ++ lex
    describe (Terminal _)   lex = "terminal " ++ lex

terminals = ['(', ')', '|']

lexer :: Lexer Token
lexer = terminal_ <|> integer_

terminal_ :: Lexer Token
terminal_ = Terminal <$> anyCharFrom terminals

integer_ :: Lexer Token
integer_ = Integer <$> digit_

pTerm :: Char -> Parser Token Char
pTerm term = 
    fromTerm <$> satisfy (\t -> (not $ isDigitToken t) && fromTerm t == term)

pInt :: Parser Token Int
pInt = fromInt <$> satisfy isDigitToken

isDigitToken (Integer _) = True
isDigitToken _           = False
