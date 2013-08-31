module Lexer where

import Prelude hiding (lex)
import CCO.Parsing (Parser(..), satisfy, Symbol(describe), (<!>))
import CCO.Lexing (Lexer(..), char, anyCharFrom, string, ignore, anyCharBut, alpha, alphaNum, lower, upper)
import CCO.SourcePos

import Control.Applicative

data Token
        = Node      {fromNode :: String}
        | Keyword   {fromKey  :: String}
        | TName    {fromTName :: String}
        | FName    {fromFName :: String}
        | BR
    deriving Show

instance Symbol Token where
    describe (Node _)          lex = "terminal " ++ lex
    describe (FName _)         lex = "field name " ++ lex
    describe (Keyword _)       lex = "keyword " ++ lex
    describe (TName _)         lex = "non-terminal / production name " ++ lex
    describe (BR)              lex = "linebreak " ++ lex

lexer :: Lexer Token
lexer =  ignore (anyCharFrom "\t ") <|> keyword_ <|> tname_ <|> fname_ <|> node_ <|> br_

{-
debug_lexer :: Lexer Token -> String -> [Token]
debug_lexer lexer string = 
    let (Symbols _ res) = lex lexer Stdin string 
        in [ t | (Token t _ _ _) <- res]
-}

keyword_ :: Lexer Token
keyword_ = Keyword <$> (foldr1 (<|>) (map string kws))
    where kws = ["Int", "Bool", "BR", "Digit" , "->", ":", "::"]

tname_ :: Lexer Token
tname_ = (\h tl -> TName (h:tl)) <$> upper <*> many (alphaNum)

fname_ = (\h tl -> FName (h:tl)) <$> lower <*> many (alphaNum)

node_ :: Lexer Token
node_ = Node <$ char '\"' <*> some (anyCharBut " \t\n") <* char '\"'

br_ :: Lexer Token
br_ = BR <$ char '\n'

pKey :: String -> Parser Token String
pKey key = 
    fromKey <$> satisfy (\t -> (isKeyToken t) && fromKey t == key) 
        <!> "keyword " ++ key
    where isKeyToken (Keyword _) = True
          isKeyToken _           = False

pTName :: Parser Token String
pTName = fromTName <$> satisfy isNameToken <!> "type / constructor name"
    where isNameToken (TName _) = True
          isNameToken _         = False

pFName :: Parser Token String
pFName = fromFName <$> satisfy isNameToken <!> "field name"
    where isNameToken (FName _) = True
          isNameToken _         = False

pNode :: Parser Token String
pNode = fromNode <$> satisfy isNodeToken <!> "node"
    where isNodeToken (Node _) = True
          isNodeToken _        = False

pBR :: Parser Token Char
pBR = '\n' <$ satisfy isBR <!> "linebreak"
    where isBR BR = True
          isBR _  = False

