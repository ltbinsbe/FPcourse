module Main where

import Gen (generate, validate)
import Parser (parser)
import CCO.Component (ioWrap, printer)
import Control.Arrow ((>>>))
import System.Environment (getArgs)
import System.Exit

main = do
        args <- getArgs
        case length args of
            2 -> do
                    let input = args !! 0
                    let path  = args !! 1 ++ "/"
                    grammar <- readFile input
                    ast    <- parser path grammar
                    case ast of 
                        Nothing  -> exitFailure
                        Just ast -> do
                            doc <- validate ast
                            case doc of
                                Nothing  -> exitFailure
                                Just doc -> generate path doc
            _ -> putStrLn "genparser [INPUT] [PATH]\nINPUT = A file containing a context-free grammar\nOUTPUT = A filepath to a directory in which program will generate files" 

