module Lexer where

import Data (terminals)
import CCO.Parsing (Parser(..), satisfy, Symbol(describe))
import CCO.Lexing (Lexer(..), digit_, anyCharFrom, string)
import Control.Applicative

data Token
        = Int       {fromInt  :: Int}
        | Bool      {fromBool :: Bool}
        | Terminal  {fromTerm :: Char}

instance Symbol Token where
    describe (Int _)        lex = "integer " ++ lex
    describe (Bool _)       lex = "boolean " ++ lex
    describe (Terminal _)   lex = "terminal " ++ lex

lexer :: Lexer Token
lexer = terminal_ <|> integer_ <|> bool_

terminal_ :: Lexer Token
terminal_ = Terminal <$> anyCharFrom terminals

integer_ :: Lexer Token
integer_ = Int <$> digit_

bool_ :: Lexer Token
bool_ = (\str -> Bool (str == "True")) <$> (string "False" <|> string "True")

pTm :: Char -> Parser Token Char
pTm term = 
    fromTerm <$> satisfy (\t -> (not $ isDigitToken t) && fromTerm t == term)

pInt :: Parser Token Int
pInt = fromInt <$> satisfy isDigitToken

pBool = fromBool <$> satisfy isBoolToken

isBoolToken (Bool _) = True
isBoolToken _        = False

isDigitToken (Int _) = True
isDigitToken _       = False
