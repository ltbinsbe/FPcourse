module Parser where

import Semantiek
import Lexer (lexer, Token, pInt, pTerm)

import qualified Prelude as P
import CCO.Component as C
import CCO.Parsing (Parser(..))
import CCO.Feedback (runFeedback)
import CCO.SourcePos (Source(..))
import Control.Applicative
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)

parser = C.parser lexer pRoot

pRoot :: Parser Token Root
pRoot = Root <$> pNode

pNode :: Parser Token Node
pNode = pLeaf <|> pNodes

pLeaf = Leaf <$> pInt
pNodes = Node <$ pTerm '(' <*> pChildren <* pTerm ')'

pChildren :: Parser Token Children
pChildren = pSome <|> pJust

pJust = Just <$> pNode
pSome = Some <$> pNode <* pTerm '|' <*> pChildren 
