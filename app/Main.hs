module Main where

import Parser
import Derivation
import Structures
import Printer
import LaTeX

import Text.Pretty.Simple

main :: IO ()
main = do 
        putStrLn "Loading laws..."
        laws <- readLaws "laws.txt"
        putStrLn "Enter an expression:"
        expString <- getLine
        let exp = parseExpression expString
            calc = simplify (calculate laws exp)
        pPrint calc
        renderToFile (makeDocument calc) "output.tex"

