module Main where

import Parser
import Derivation
import Structures
import Printer
import LaTeX

import Text.Pretty.Simple
import Text.Megaparsec

main :: IO ()
main = do 
        putStrLn "Loading laws..."
        laws <- readLaws "laws.txt"
        putStrLn "Enter an expression:"
        expString <- getLine
        putStrLn "Enter desired output file name:"
        outputFileString <- getLine
        case parse (expression <* eof) "" expString of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right exp -> do
                            let calc = simplify (calculate laws exp)
                            pPrint calc
                            renderToFile (makeDocument calc) (outputFileString ++ ".tex")

