module Lexer where

import Prelude hiding (lex)
import CCO.Parsing (Parser(..), satisfy, Symbol(describe), (<!>))
import CCO.Lexing (Lexer(..), char, anyCharFrom, anyChar, string, ignore, anyCharBut, alpha, alphaNum, lower, upper, lex, LexicalUnit(..), Symbols(..))
import CCO.SourcePos (Source(..))

import Control.Applicative

data Token
        = Node      {fromNode :: String}
        | Keyword   {fromKey  :: String}
        | Quant    {fromQuant :: String}
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
    describe (Quant _)         lex = "quantifier " ++ lex

lexer :: Lexer Token
lexer = ignore (string "--" *> many (anyCharBut "\n")) <|> 
        ignore (anyCharFrom "\t ") <|> 
        keyword_ <|> 
        quant_ <|> 
        tname_ <|> 
        fname_ <|> 
        node_ <|> 
        br_

debug_lexer :: Lexer a -> String -> [a]
debug_lexer lexer string = 
    let (Symbols _ res) = lex lexer Stdin string 
        in [ t | (Token t _ _ _) <- res]


keyword_ :: Lexer Token
keyword_ = Keyword <$> (foldr1 (<|>) (map string kws))
    where kws = [ "Int", "Bool", "BR"
                , "Digit" , "->", ":", "::"
                ]

tname_ :: Lexer Token
tname_ = (\h tl -> TName (h:tl)) <$> upper <*> many (alphaNum)

fname_ = (\h tl -> FName (h:tl)) <$> lower <*> many (alphaNum)

node_ :: Lexer Token
node_ = Node <$ char '\"' <*> some (anyCharBut " \t\n") <* char '\"'

br_ :: Lexer Token
br_ = BR <$ char '\n'

quant_ = Quant . (:[]) <$> anyCharFrom "?+*"

pKey :: String -> Parser Token String
pKey key = 
    fromKey <$> satisfy (\t -> (isKeyToken t) && fromKey t == key) 
        <!> "keyword " ++ key
    where isKeyToken (Keyword _) = True
          isKeyToken _           = False

pTName = fromTName <$> satisfy isNameToken <!> "type / constructor name"
    where isNameToken (TName _) = True
          isNameToken _         = False

pFName = fromFName <$> satisfy isNameToken <!> "field name"
    where isNameToken (FName _) = True
          isNameToken _         = False

pNode = fromNode <$> satisfy isNodeToken <!> "node"
    where isNodeToken (Node _) = True
          isNodeToken _        = False

pBR = '\n' <$ satisfy isBR <!> "linebreak"
    where isBR BR = True
          isBR _  = False

pQuant = fromQuant <$> satisfy isQuantToken <!> "quantifier"
    where isQuantToken (Quant _) = True
          isQuantToken _         = False
