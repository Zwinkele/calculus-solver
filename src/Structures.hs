module Structures where

{-
    Things to consider:
        - Function application looks a lot like multiplication i.e. is y(x) the same as y*x or y applied to x?
            - Possible solution: treat y(x) as function application always, require explicit * for multiplication.
        - Treating + and * as strictly binary operations might make simplification annoying.
        - Maybe we want constants to be instances of Real instead of just Ints.
        - Unsure what d/dx(y) should be.
            - Possible solution: Treat non-x variables as constants unless they're functions, so d/dx(y) = 0, but d/dx(y(x)) = y'(x).
        - The assignment "Submit base project" description implies we might choose different data structures
            to represent input functions and output derivatives, but I don't see why that would be necessary.
        - The assignment "Demonstrate decent reasoning" description implies we should be able to evaluate d/dx(y^x) and d/dz(x^y), which means
            we might need to add arbitrary powers and logarithms to our expression data structure.
-}

data Variable = Variable String
    deriving (Eq, Show)

data Expression = Constant Int -- 1, 2, 91
                | Reference Variable -- x, y, var
                | Application Variable Expression -- f(x), g(x), z(3^x)
                | Binary Operation Expression Expression -- 3+y, x*y, 1/x
                | Exponent Expression Int -- x^2, y^4, z^0
                | Derivative Expression -- d/dx(x^2), d/dx(y)
    deriving Show

data Operation = Add | Sub | Mul | Div
    deriving (Eq, Show)
