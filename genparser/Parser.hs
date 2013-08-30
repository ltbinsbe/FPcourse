module Parser where

import Gen
import Lexer

import CCO.Component as C
import CCO.Parsing (Parser(..), manySepBy, someSepBy, eof)
import Control.Applicative hiding (some, many)

parser = C.parser lexer pGrammar

pGrammar = Grammar <$> some (pProduction <* pLB) <* eof

pProduction = 
    (\cs nt rhs -> Production (NT nt) cs rhs) <$> 
                pName <*
                pKey ":" <*>
                pName <* 
                pKey "->" <*> 
                (some (NT       <$> pName       <|> 
                       T        <$> pNode       <|> 
                       IntLit   <$  pKey "Int"  <|> 
                       BoolLit  <$  pKey "Bool"))

many p = (:) <$> p <*> many p <|> pure []
some p = (:) <$> p <*> many p
