module Main where

import Gen (generate, validate)
import Parser (parser)
import CCO.Component (ioWrap, printer)
import Control.Arrow ((>>>))

main = ioWrap $ parser >>> validate >>> generate 
