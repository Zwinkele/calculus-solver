import Structures
import Parser
import Derivation
import Text.Megaparsec
import Data.MultiSet

main :: IO ()
main = do {
    putStrLn "Example data:";
    putStrLn (show example1);
    putStrLn (show example2);
    putStrLn (show example3);
    putStrLn "\n";
    putStrLn "Example parsing:";
    parseTest law "commutativity : x : x+1 = 1+x";
    parseTest law "associativity : x,y,z : x*(y*z) = (x*y)*z";
    putStrLn (show (calculate (Constant 3)))}

-- f(x)
example1 = 
    Application 
        (Variable "f") 
        (Reference (Variable "x"))

-- 1+2+3
example2 = 
    ACOperation 
        Add 
        [(Constant 1), (Constant 2), (Constant 3)]

-- d/dz(z^2)
example3 = 
    Derivative 
    (Variable "z") 
    (BinaryOperation 
        Pow 
        (Reference (Variable "z")) 
        (Constant 2))
