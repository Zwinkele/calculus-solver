import Structures
import Parser
import Derivation
import Printer

import Text.Megaparsec hiding (match)
import Data.MultiSet
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

main :: IO ()
main = defaultMain (testGroup "All Tests" [expressionParsingTests, lawParsingTests, rewritingTests])

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

lawParsingTests = testGroup "Law Parsing Tests" [lpt1]
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
                ([
                    (Law "commutativity of +" [Variable "a", Variable "b"] 
                        (ACOperation Add [
                            Reference (Variable "a"),
                            Reference (Variable "b")],
                        ACOperation Add [
                            Reference (Variable "b"),
                            Reference (Variable "a")]))]))})

rewritingTests = testGroup "Rewriting Tests" [rwt1, rwt2]
rwt1 = testCase "d/dx(4-3) = d/dx(4)-d/dx(3)"
        (assertEqual ""
            (Just (Step "test law" 
                (BinaryOperation Sub 
                    (Derivative (Variable "x") (Constant 4)) 
                    (Derivative (Variable "x") (Constant 3)))))
            (rewrite 
                (Law "test law" [Variable "a", Variable "b"]
                    (Derivative (Variable "x")
                        (BinaryOperation Sub 
                            (Reference (Variable "a"))
                            (Reference (Variable "b"))),
                    (BinaryOperation Sub 
                        (Derivative (Variable "x") (Reference (Variable "a")))
                        (Derivative (Variable "x") (Reference (Variable "b"))))))
                (Derivative (Variable "x")
                    (BinaryOperation Sub (Constant 4) (Constant 3)))))
rwt2 = testCase "substitution x=5"
        (assertEqual ""
            (Just (Step "test law" 
                (Constant 5)))
            (rewrite 
                (Law "test law" []
                    (Reference (Variable "x"),
                    Constant 5))
                (Reference (Variable "x"))))

-- some test data
expr = ACOperation Add exps
exp1 = ACOperation Mul [
            Derivative (Variable "x") (
                Reference (Variable "a")),
            Reference (Variable "b"),
            Reference (Variable "c")]
exp2 = ACOperation Mul [
            Reference (Variable "a"),
            Derivative (Variable "x") (
                ACOperation Mul [
                    Reference (Variable "b"),
                    Reference (Variable "c")])]
exps = [exp1, exp2]
pattern = Derivative (Variable "x") (
        ACOperation Mul [
            Reference (Variable "a"),
            Reference (Variable "b")])
vars = [Variable "a",Variable "b",Variable "x"]
replacement = ACOperation Add [
        ACOperation Mul [
            Derivative (Variable "x") (
                Reference (Variable "a")),
            Reference (Variable "b")],
        ACOperation Mul [
            Reference (Variable "a"),
            Derivative (Variable "x") (
                Reference (Variable "b"))]]
productRule = Law "product rule" vars (pattern, replacement)
