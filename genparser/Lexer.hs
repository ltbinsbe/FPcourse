module Lexer where

import Prelude hiding (lex)
import Gen
import CCO.Parsing (Parser(..), satisfy, Symbol(describe), (<!>))
import CCO.Lexing (Lexer(..), char, anyCharFrom, string, ignore, anyCharBut, alpha, alphaNum)
import CCO.SourcePos

import Control.Applicative

data Token
        = Node      {fromNode :: String}
        | Keyword   {fromKey  :: String}
        | Name      {fromName :: String}
        | LB
    deriving Show

instance Symbol Token where
    describe (Node _)           lex = "terminal " ++ lex
    describe (Keyword _)        lex = "keyword " ++ lex
    describe (Name _)           lex = "name " ++ lex
    describe (LB)               lex = "linebreak " ++ lex

lexer :: Lexer Token
lexer =  ignore (anyCharFrom "\t ") <|> keyword_ <|> name_ <|> node_ <|> lb_

{-
debug_lexer :: Lexer Token -> String -> [Token]
debug_lexer lexer string = 
    let (Symbols _ res) = lex lexer Stdin string 
        in [ t | (Token t _ _ _) <- res]
-}

keyword_ :: Lexer Token
keyword_ = Keyword <$> (foldr1 (<|>) (map string kws))
    where kws = ["Int", "Bool", "->", ":"]

name_ :: Lexer Token
name_ = (\h tl -> Name (h:tl)) <$> alpha <*> many (alphaNum)

node_ :: Lexer Token
node_ = Node <$ char '\"' <*> some (anyCharBut " \t\n") <* char '\"'

lb_ :: Lexer Token
lb_ = LB <$ char '\n'

pKey :: String -> Parser Token String
pKey key = 
    fromKey <$> satisfy (\t -> (isKeyToken t) && fromKey t == key) 
        <!> "keyword " ++ key
    where isKeyToken (Keyword _) = True
          isKeyToken _           = False

pName :: Parser Token String
pName = fromName <$> satisfy isNameToken <!> "name"
    where isNameToken (Name _) = True
          isNameToken _        = False

pNode :: Parser Token String
pNode = fromNode <$> satisfy isNodeToken <!> "node"
    where isNodeToken (Node _) = True
          isNodeToken _        = False

pLB :: Parser Token Char
pLB = '\n' <$ satisfy isLB <!> "linebreak"
    where isLB LB = True
          isLB _  = False

