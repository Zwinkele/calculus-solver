import Structures
import Parser
import Derivation

import Text.Megaparsec
import Data.MultiSet
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "All Tests" [expressionParsingTests, lawParsingTests, calculationTests])

expressionParsingTests = testGroup "Expression Parsing Tests" [ept1, ept2, ept3]
ept1 = testCase "f(x)" 
        (assertEqual "" 
            (Just (Application 
                (Variable "f") 
                (Reference (Variable "x"))))
            (parseMaybe expression "f(x)"))
ept2 = testCase "1+2+3"
        (assertEqual ""
            (Just (ACOperation Add [
                (Constant 1), 
                (Constant 2),
                (Constant 3)]))
            (parseMaybe expression "1+2+3"))
ept3 = testCase "d/dz(z^2)"
        (assertEqual ""
            (Just (Derivative 
                (Variable "z") 
                (BinaryOperation Pow 
                    (Reference (Variable "z")) 
                    (Constant 2))))
            (parseMaybe expression "d/dz(z^2)"))

lawParsingTests = testGroup "Law Parsing Tests" [lpt1, lpt2]
lpt1 = testCase "parsing laws from strings"
        (assertEqual ""
            (Just (Law "commutativity of +" [Variable "a", Variable "b"] 
                (ACOperation Add [
                    Reference (Variable "a"),
                    Reference (Variable "b")],
                ACOperation Add [
                    Reference (Variable "b"),
                    Reference (Variable "a")])))
            (parseMaybe law "commutativity of + : a,b : a + b = b + a"))
lpt2 = testCase "reading laws from a file" 
        (do {laws <- (readLaws "test/laws.txt");
            assertBool "" (laws ==
                [(Just (Law "commutativity of +" [Variable "a", Variable "b"] 
                (ACOperation Add [
                    Reference (Variable "a"),
                    Reference (Variable "b")],
                ACOperation Add [
                    Reference (Variable "b"),
                    Reference (Variable "a")])))])})

calculationTests = testGroup "Calculation Tests" [ct1]
ct1 = testCase "empty calculation of x"
        (assertEqual ""
            (Calc (Reference (Variable "x")) [])
            (calculate (Reference (Variable "x"))))
