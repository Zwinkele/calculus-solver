module Structures where
import Data.Ratio
import Data.List

data Variable = Variable String
    deriving (Eq, Ord, Show)

data Expression = Constant Rational -- 8%1, 2%3, 91%7
                | Reference Variable -- x, y, var
                | BinaryOperation BinOp Expression Expression -- x-y, 1/x x^2
                | ACOperation ACOp [Expression] -- a+3+7+3, x*y*x
                | Application Variable Expression -- f(x), g(x), z(3^x)
                | Derivative Variable Expression -- d/dx(x^2), d/dx(y)
    deriving (Ord)

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

acOpEq :: (Eq a) => [a] -> [a] -> Bool
acOpEq xs ys = null (xs \\ ys) && null (ys \\ xs)

instance Eq Expression where
    Constant n == Constant y = n == y
    Reference v == Reference w = v == w
    BinaryOperation op e1 e2 == BinaryOperation op' e1' e2' = (op == op') && (e1 == e1') && (e2 == e2')
    ACOperation op exps == ACOperation op' exps' = (op == op') && (acOpEq exps exps')
    Application v exp == Application v' exp' = (v == v') && (exp == exp')
    Derivative v exp == Derivative v' exp' = (v == v') && (exp == exp')
    x == y = False
