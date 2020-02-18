module Structures where

import Data.MultiSet

data Variable = Variable String
    deriving (Eq, Ord, Show)

data Expression = Constant Int -- 1, 2, 91
                | Reference Variable -- x, y, var
                | Application Variable Expression -- f(x), g(x), z(3^x)
                | BinaryOperation BinOp Expression Expression -- x-y, 1/x x^2
                | ACOperation ACOp (MultiSet Expression) -- a+3+7+3, x*y*x
                | Derivative Variable Expression -- d/dx(x^2), d/dx(y)
    deriving (Eq, Ord, Show)

data BinOp = Sub | Div | Pow
    deriving (Eq, Ord, Show)

data ACOp = Add | Mul
    deriving (Eq, Ord, Show)

data Law = Law String Equation
    deriving (Eq, Ord, Show)

type Equation = (Expression, Expression)

data Calculation = Calc Expression [Step]
    deriving (Eq, Ord, Show)

data Step = Step String Expression
    deriving (Eq, Ord, Show)
