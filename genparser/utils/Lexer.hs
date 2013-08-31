module Lexer where

import Prelude hiding (lex)
import Sem
import Data (terminals)
import CCO.Parsing (Parser(..), satisfy, Symbol(describe), (<!>))
import CCO.Lexing (Lexer(..), digit_, anyCharFrom, string, ignore, anyCharBut, char, lex, Symbols(..), LexicalUnit(..))
import CCO.SourcePos (Source(..))
import Control.Applicative

data Token
        = Digit   {fromDigit  :: Int}
        | Bool      {fromBool :: Bool}
        | Terminal  {fromTerm :: String}
        | BR
        deriving Show

instance Symbol Token where
    describe (Digit _)      lex = "digit " ++ lex
    describe (Bool _)       lex = "boolean " ++ lex
    describe (Terminal _)   lex = "terminal " ++ lex
    describe (BR)              lex = "linebreak " ++ lex

lexer :: Lexer Token
lexer = terminal_ <|> mydigit_ <|> bool_ <|> br_ <|> ignore (anyCharFrom " \t")

debug_lexer :: Lexer Token -> String -> [Token]
debug_lexer lexer string = 
    let (Symbols _ res) = lex lexer Stdin string 
        in [ t | (Token t _ _ _) <- res]

terminal_ :: Lexer Token
terminal_ = Terminal <$> foldr1 (<|>) (map string terminals)

mydigit_ :: Lexer Token
mydigit_ = Digit <$> digit_

bool_ :: Lexer Token
bool_ = (\str -> Bool (str == "True")) <$> (string "False" <|> string "True")

br_ :: Lexer Token
br_ = BR <$ char '\n'

pTm :: String -> Parser Token String
pTm term = 
    fromTerm <$> satisfy (\t -> (isTermToken t) && fromTerm t == term)
        <!> "terminal " ++ term
    where isTermToken (Terminal _) = True
          isTermToken _            = False

pDigit = fromDigit <$> satisfy isDigitToken <!> "digit"
    where isDigitToken (Digit _) = True
          isDigitToken _         = False

pInt :: Parser Token Int
pInt = (foldr1 (\d ds -> d + 10 * ds)) . reverse <$> some pDigit

pBool = fromBool <$> satisfy isBoolToken <!> "boolean"
    where isBoolToken (Bool _)   = True
          isBoolToken _          = False

pBR :: Parser Token ()
pBR = () <$ satisfy isBR <!> "linebreak"
    where isBR BR = True
          isBR _  = False

