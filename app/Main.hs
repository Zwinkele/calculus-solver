module Main where

import Parser
import Derivation

import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do 
        putStrLn "Loading laws..."
        laws <- readLaws "src/laws.txt"
        putStrLn "Enter an expression:";
        expString <- getLine
        let exp = parseExpression expString
            calc = calculate laws exp
        pPrint calc

