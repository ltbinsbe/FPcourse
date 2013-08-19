module Voorbeeld where

import Semantiek (som, height)
import Parser (parser)

import CCO.Component (printer, ioWrap)
import Control.Arrow ((>>>))

mainSom    = ioWrap $ parser >>> som    >>> printer
mainHeight = ioWrap $ parser >>> height >>> printer
