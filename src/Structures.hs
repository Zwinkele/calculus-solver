module Structures where
import Data.Ratio

data Variable = Variable String
    deriving (Eq, Ord, Show)

data Expression = Constant Rational -- 8%1, 2%3, 91%7
                | Reference Variable -- x, y, var
                | BinaryOperation BinOp Expression Expression -- x-y, 1/x x^2
                | ACOperation ACOp [Expression] -- a+3+7+3, x*y*x
                | Application Variable Expression -- f(x), g(x), z(3^x)
                | Derivative Variable Expression -- d/dx(x^2), d/dx(y)
    deriving (Eq, Ord)

data BinOp = Sub | Div | Pow
    deriving (Eq, Ord, Show)

data ACOp = Add | Mul
    deriving (Eq, Ord, Show)

data Law = Law String [Variable] Equation
    deriving (Eq, Ord)

type Equation = (Expression, Expression)

data Calculation = Calc Expression [Step]
    deriving (Eq, Ord)

data Step = Step String Expression
    deriving (Eq, Ord)

type Substitution = [(Variable, Expression)]
