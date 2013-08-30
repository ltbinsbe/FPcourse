module Lexer where

import Sem
import Data (terminals)
import CCO.Parsing (Parser(..), satisfy, Symbol(describe), (<!>))
import CCO.Lexing (Lexer(..), digit_, anyCharFrom, string, ignore, anyCharBut)
import Control.Applicative

data Token
        = Int       {fromInt  :: Int}
        | Bool      {fromBool :: Bool}
        | Terminal  {fromTerm :: String}

instance Symbol Token where
    describe (Int _)        lex = "integer " ++ lex
    describe (Bool _)       lex = "boolean " ++ lex
    describe (Terminal _)   lex = "terminal " ++ lex

lexer :: Lexer Token
lexer = terminal_ <|> integer_ <|> bool_ <|> ignore (anyCharFrom " \t\n")

terminal_ :: Lexer Token
terminal_ = Terminal <$> foldr1 (<|>) (map string terminals)

integer_ :: Lexer Token
integer_ = Int . (foldr1 (\d ds -> d + 10 * ds)) <$> some digit_

bool_ :: Lexer Token
bool_ = (\str -> Bool (str == "True")) <$> (string "False" <|> string "True")

pTm :: String -> Parser Token String
pTm term = 
    fromTerm <$> satisfy (\t -> (not $ isDigitToken t) && fromTerm t == term)
        <!> "terminal " ++ term

pInt :: Parser Token Int
pInt = fromInt <$> satisfy isDigitToken <!> "integer"

pBool = fromBool <$> satisfy isBoolToken <!> "boolean"

isBoolToken (Bool _) = True
isBoolToken _        = False

isDigitToken (Int _) = True
isDigitToken _       = False
