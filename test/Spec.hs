import Structures
import Parser
import Derivation
import Printer

import Text.Megaparsec hiding (match)
import Data.MultiSet
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.Ratio

main :: IO ()
main = defaultMain (testGroup "All Tests" [expressionParsingTests, lawParsingTests, rewritingTests, specialSimplificationTests])

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
specialSimplificationTests = testGroup "Special Simplification Tests" [sst1,sst2,sst3,sst4,sst5,sst6,sst7]
sst1 = testCase "Combine like addition terms"
        (assertEqual ""
            (combineLikeAddTerms (ACOperation Add [(ACOperation Mul [Constant 3, Reference (Variable "x")]), (ACOperation Mul [Reference (Variable "x"), Constant 4])]))
            (ACOperation Add [ACOperation Mul [ACOperation Add [Constant (3 % 1),Constant (4 % 1)],Reference (Variable "x")]])
        )
sst2 = testCase "Combine like multiplication terms"
        (assertEqual ""
            (combineLikeMulTerms (ACOperation Mul [(BinaryOperation Pow (Reference (Variable "x")) (Constant 2)), (BinaryOperation Pow (Reference (Variable "x")) (Constant 3))]))
            (ACOperation Mul [BinaryOperation Pow (Reference (Variable "x")) (ACOperation Add [Constant (2 % 1),Constant (3 % 1)])])
        )
sst3 = testCase "Unwrap ACOperation"
        (assertEqual ""
            (unwrapACOp (ACOperation Add [Constant 3]))
            (Constant (3 % 1))
        )
sst4 = testCase "Combine nested ACOperations"
        (assertEqual ""
            (combineACOp (ACOperation Add [(Constant 3), (ACOperation Add [Constant 4, Constant 2])]))
            (ACOperation Add [Constant (3 % 1),Constant (4 % 1),Constant (2 % 1)])
        )
sst5 = testCase "d/dx(x) = 1"
        (assertEqual ""
            (simplifyDerivatives (Derivative (Variable "x") (Reference (Variable "x"))))
            (Constant (1 % 1))
        )
sst6 = testCase "d/dx(constant) = 0"
        (assertEqual ""
            (simplifyDerivatives (Derivative (Variable "x") (Constant 3)))
            (Constant (0 % 1))
        )
sst7 = testCase "Simplify Constant Math"
        (assertEqual ""
            (simplifyConstMath (ACOperation Add [BinaryOperation Sub (Constant 3) (Constant 2), BinaryOperation Pow (Constant 2) (Constant 3), BinaryOperation Div (Constant 8) (Constant 2), ACOperation Add [Constant 3, Constant 2], ACOperation Mul [Constant 0, Constant 3]]))
            (ACOperation Add [Constant (1 % 1),Constant (8 % 1),Constant (4 % 1),ACOperation Add [Constant (5 % 1)],Constant (0 % 1)])
        )

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
