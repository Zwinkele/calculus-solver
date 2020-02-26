{-# LANGUAGE StandaloneDeriving #-}

module Printer where

import Structures
import Data.List
    
acOpToString :: ACOp -> String
acOpToString op = case op of
    Add -> "+"
    Mul -> "*"

binOpToString :: BinOp -> String
binOpToString op = case op of
    Sub -> "-"
    Div -> "/"
    Pow -> "^"

expToString :: Expression -> String
expToString exp = case exp of
    Constant n -> show n
    Reference (Variable varName) -> varName
    BinaryOperation op exp1 exp2 -> (expToString exp1) ++ (binOpToString op) ++ (expToString exp2)
    ACOperation op exps -> intercalate (acOpToString op) (map expToString exps)
    Application (Variable varName) exp' -> varName ++ "(" ++ (expToString exp') ++ ")"
    Derivative (Variable varName) exp' -> "d/d" ++ varName ++ "(" ++ (expToString exp') ++ ")"

instance Show Expression where
    show = expToString

deriving instance Show Step

deriving instance Show Calculation

deriving instance Show Law
