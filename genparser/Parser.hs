module Parser where

import Gen
import Lexer

import CCO.Component (component)
import CCO.Feedback  (runFeedback)
import CCO.Parsing (Parser(..), manySepBy, someSepBy, eof, parse_)
import CCO.SourcePos (Source(..))
import Control.Applicative 
import System.IO (stderr)

parser path gm = runFeedback (parse_ lexer pGrammar (File path) gm) 1 1 stderr

-- component (\stdid -> parse_ lexer pGrammar (File path) gm) 

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

