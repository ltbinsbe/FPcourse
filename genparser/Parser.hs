module Parser where

import AG.Main
import Lexer hiding (Token(BR))

import CCO.Component (component)
import CCO.Feedback  (runFeedback)
import CCO.Parsing (Parser(..), manySepBy, someSepBy, eof, parse_)
import CCO.SourcePos (Source(..))
import Control.Applicative 
import System.IO (stderr)

parser path gm = runFeedback (parse_ lexer pGrammar (File path) gm) 1 1 stderr

-- component (\stdid -> parse_ lexer pGrammar (File path) gm) 

pGrammar = Grammar <$> some (pProduction <* pBR) <* eof

pProduction = 
    (\cs nt rhs -> Production (NT nt nt) cs rhs) <$> 
                pTName <*
                pKey ":" <*>
                pTName <* 
                pKey "->" <*> 
                (some (flip NT  <$> pFName <* pKey "::" <*> pTName      <|> 
                       T        <$> pNode                               <|> 
                       IntLit   <$> pFName <* pKey "::" <* pKey "Int"   <|> 
                       DigitLit <$> pFName <* pKey "::" <* pKey "Digit" <|> 
                       BR       <$  pKey "BR"                           <|> 
                       BoolLit  <$> pFName <* pKey "::" <* pKey "Bool"))

