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

pGrammar = Grammar <$> someSepBy (some pBR) pProduction <* many pBR <* eof

pProduction = 
    (\cs nt rhs -> Production (NT nt nt Single) cs rhs) <$> 
        pTName <*
        pKey ":" <*>
        pTName <* 
        pKey "->" <*> 
        (some ( 
          pNTField cNT <|> 
          pIField cIL  <|> 
          pDField cDL  <|> 
          pBField cBL  <|>
          pBRField cBR <|> 
          pTField cT )   
        )
    where   
      cBR        = BR           . c2q
      cBL n      = BoolLit n    . c2q
      cDL n      = DigitLit n   . c2q
      cIL n      = IntLit n     . c2q
      cNT f t    = NT t f       . c2q
      cT  i      = T i          . c2q
      pNTField c = c <$> pFName <* pKey "::" <*> pTName <*> 
                            (pQuant <|> pure "")
      pIField  c = c <$> pFName <* pKey "::" <* pKey "Int" <*> 
                            (pQuant <|> pure "")
      pDField  c = c <$> pFName <* pKey "::" <* pKey "Digit" <*> 
                            (pQuant <|> pure "")
      pBField  c = c <$> pFName <* pKey "::" <* pKey "Bool" <*> 
                            (pQuant <|> pure "")
      pBRField c = c <$  pKey "BR" <*> (pure "" <|> pQuant)
      pTField c  = c <$> pNode <*> (pure ""<|> pQuant)
      c2q c    = case c of
                  ""  -> Single
                  "+" -> Some
                  "*" -> Many
                  "?" -> Optional
