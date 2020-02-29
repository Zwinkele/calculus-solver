module Parser where

import Structures
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (binary, space)
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Maybe
import Data.Ratio

type Parser = Parsec Void String

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

expression :: Parser Expression
expression = makeExprParser term table <?> "expression"

term :: Parser Expression
term = space *> (parens expression <|> constant <|> derivative <|> try function <|> ref) <* space <?> "term"

constant :: Parser Expression
constant = do {n <- some digitChar;
               return (Constant ((read n :: Integer)%1))}
           <?> "constant"

derivative :: Parser Expression
derivative = do {_ <- string "d/d";
                 v <- var;
                 expr <- parens expression;
                 return (Derivative v expr)}

function :: Parser Expression
function = do {name <- some letterChar;
               input <- parens expression;
               return (Application (Variable name) input)}

var :: Parser Variable
var = do {name <- some letterChar;
          return (Variable name)}

ref :: Parser Expression
ref = do {v <- var;
          return (Reference v)}

minExpr a = mulExpr (Constant (-1)) a
addExpr (ACOperation Add as) (ACOperation Add bs) = ACOperation Add (as++bs)
addExpr (ACOperation Add as) b = ACOperation Add (as++[b])
addExpr a (ACOperation Add bs) = ACOperation Add (a:bs)
addExpr a b = ACOperation Add [a,b]
subExpr a b = BinaryOperation Sub a b
mulExpr (ACOperation Mul as) (ACOperation Mul bs) = ACOperation Mul (as++bs)
mulExpr (ACOperation Mul as) b = ACOperation Mul (as++[b])
mulExpr a (ACOperation Mul bs) = ACOperation Mul (a:bs)
mulExpr a b = ACOperation Mul [a,b]
divExpr a b = BinaryOperation Div a b
powExpr a b = BinaryOperation Pow a b

binary  name f = InfixL  (f <$ string name)
prefix  name f = Prefix  (f <$ string name)

table = 
    [ [ binary "^" powExpr],
      [ prefix "+" id,
        prefix "-" minExpr ],
      [ binary "*" mulExpr,
        binary "/" divExpr ],
      [ binary "+" addExpr,
        binary "-" subExpr ] ]

law :: Parser Law
law = do {name <- (someTill asciiChar (try (space *> (char ':'))));
          _ <- space;
          freeVars <- sepBy var (char ',');
          _ <- space *> (char ':') <* space;
          eq <- equation;
          return (Law name freeVars eq)}

equation :: Parser Equation
equation = do {lhs <- expression;
               _ <- space *> (char '=') <* space;
               rhs <- expression;
               return (lhs, rhs)}

parseExpression :: String -> Expression
parseExpression s = fromJust (parseMaybe expression s)

readLaws :: String -> IO ([Law])
readLaws location = do {text <- readFile location;
                        return (catMaybes (map (parseMaybe law) (lines text)))}