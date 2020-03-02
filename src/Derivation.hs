module Derivation where

import Structures
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ratio
import qualified Data.Map.Strict as Map

calculate :: [Law] -> Expression -> Calculation
calculate laws exp = Calc exp (calculateSteps laws exp)

calculateSteps :: [Law] -> Expression -> [Step]
calculateSteps laws exp = 
    case s of
        Nothing -> []
        Just (Step name newExp) -> (Step name newExp):(calculateSteps laws newExp)
    where s = step laws exp

step :: [Law] -> Expression -> Maybe Step
step laws exp = join (find isJust (map (\law -> rewrite law exp) laws))

rewrite :: Law -> Expression -> Maybe Step
rewrite (Law name vars (pattern, replacement)) exp = 
    fmap (\newExp -> Step name newExp) (findMatch f exp)
        where f exp = fmap (\sub -> apply sub replacement) (match pattern vars exp)

findMatch :: (Expression -> Maybe Expression) -> Expression -> Maybe Expression
findMatch f exp =
    case e of
        Nothing -> case exp of
            Constant n -> Nothing
            Reference var -> Nothing
            BinaryOperation op exp1 exp2 -> case match1 of
                Nothing -> fmap (\matchedExp -> BinaryOperation op exp1 matchedExp) (findMatch f exp2)
                (Just matchedExp) -> Just (BinaryOperation op matchedExp exp2)
                where match1 = findMatch f exp1
            ACOperation op exps -> fmap (ACOperation op) (matchList f exps)
            Application v exp' -> fmap (Application v) (f exp')
            Derivative v exp' -> fmap (Derivative v) (f exp')
        _ -> e
        where e = f exp

matchList :: (Expression -> Maybe Expression) -> [Expression] -> Maybe [Expression]
matchList f [] = Nothing
matchList f (exp:exps) = 
    case match of
        Nothing -> fmap (exp:) (matchList f exps)
        (Just newExp) -> Just (newExp:exps)
    where match = findMatch f exp

-- returns a substitution if the top-level expression matches the pattern
match :: Expression -> [Variable] -> Expression -> Maybe Substitution
match pattern vars exp = 
    case (pattern, exp) of
        (Constant n, Constant m) ->
            if m == n
            then (Just [])
            else Nothing
        (Reference v, _) -> 
            if v `elem` vars
            then Just [(v, exp)]
            else case exp of
                Reference v' ->
                    if v == v'
                    then (Just [])
                    else Nothing
                _ -> Nothing
        (BinaryOperation op exp1 exp2, BinaryOperation op' exp1' exp2') ->
            if (op == op')
            then (++) <$> (match exp1 vars exp1') <*> (match exp2 vars exp2')
            else Nothing
        (ACOperation op exps, ACOperation op' exps') -> 
            if op == op'
            then matchACOp op exps vars exps'
            else Nothing
        (Application v innerPattern, Application v' innerExp) ->
            if v == v'
            then match innerPattern vars innerExp
            else Nothing
        (Derivative v innerPattern, Derivative v' innerExp) ->
            pure ((v, (Reference v')):) <*> (match innerPattern vars innerExp)
        (_, _) -> Nothing

matchACOp :: ACOp -> [Expression] -> [Variable] -> [Expression] -> Maybe Substitution
matchACOp op (exp:[]) vars (exp':[]) = match exp vars exp'
matchACOp op (exp:[]) vars exps' = match exp vars (ACOperation op exps')
matchACOp op (exp:exps) vars (exp':exps') = (++) <$> (match exp vars exp') <*> (matchACOp op exps vars exps')
matchACOp _ _ _ _ = Nothing

apply :: Substitution -> Expression -> Expression
apply sub replacement = 
    case replacement of
        Constant n -> Constant n
        Reference v -> subForExpr sub v
        BinaryOperation op exp1 exp2 -> BinaryOperation op (apply sub exp1) (apply sub exp2)
        ACOperation op exps -> ACOperation op (map (apply sub) exps)
        Application v exp -> Application v (apply sub exp)
        Derivative v exp -> Derivative (subForVar sub v) (apply sub exp)

subForExpr :: Substitution -> Variable -> Expression
subForExpr [] v = Reference v
subForExpr ((v',exp):restOfSubs) v =
    if v == v'
    then exp
    else subForExpr restOfSubs v

subForVar :: Substitution -> Variable -> Variable
subForVar [] v = v
subForVar ((v', Reference v''):restOfSubs) v =
    if v == v'
    then v''
    else subForVar restOfSubs v
subForVar (_:restOfSubs) v = subForVar restOfSubs v

-- Composition of simplification functions
simplifyFunc :: Calculation -> Calculation
simplifyFunc = (simplifyStep "Combine like addition terms" (combineLikeAddTerms . combineACOp)) .
               (simplifyStep "Combine like multiplication terms" (combineLikeMulTerms . combineACOp)) .
               (simplifyStep "Unwrapping ACOperations with single expression" unwrapACOp) . 
               (simplifyStep "Simplify Constant Math" simplifyConstMath) . 
               (simplifyStep "Simplify d/dx(x) = 1 and d/dx(constant) = 0" simplifyDerivatives) . 
               (simplifyStep "combine nested ACOperations" combineACOp)

simplify :: Calculation -> Calculation
simplify calc = 
    if finalExpression calc /= finalExpression newCalc
        then simplify newCalc
        else calc
    where newCalc = simplifyFunc calc

-- Creates a simplification step using a simplifying function
simplifyStep :: String -> (Expression -> Expression) -> Calculation -> Calculation
simplifyStep stepName simplifyFunc c@(Calc exp steps) = 
    if finalExpression c == finalExpression newCalc
        then c
        else newCalc
    where newCalc = Calc exp (steps ++ [Step stepName (simplifyFuncRepeat simplifyFunc (finalExpression c))])

finalExpression :: Calculation -> Expression
finalExpression (Calc exp []) = exp
finalExpression (Calc exp steps) = getExpFromStep (last steps)

getExpFromStep :: Step -> Expression
getExpFromStep (Step s exp) = exp

-- Repeatedly call the simplifyFunc until nothing changes
simplifyFuncRepeat :: (Expression -> Expression) -> Expression -> Expression
simplifyFuncRepeat simplifyFunc exp =
    if exp /= newExp
        then simplifyFuncRepeat simplifyFunc newExp
        else exp
    where newExp = simplifyFunc exp

combineLikeAddTerms :: Expression -> Expression
combineLikeAddTerms (Constant n) = Constant n
combineLikeAddTerms (Reference v) = Reference v
combineLikeAddTerms (BinaryOperation op exp1 exp2) = BinaryOperation op (combineLikeAddTerms exp1) (combineLikeAddTerms exp2)
combineLikeAddTerms (ACOperation Add exps) = ACOperation Add (combineAddExps exps [])
combineLikeAddTerms (ACOperation Mul exps) = ACOperation Mul (map combineLikeAddTerms exps)
combineLikeAddTerms (Application v exp) = Application v (combineLikeAddTerms exp)
combineLikeAddTerms (Derivative v exp) = Derivative v (combineLikeAddTerms exp)

-- Accum pairs are (Exps, Coefficient)
combineAddExps :: [Expression] -> [([Expression],Expression)] -> [Expression]
combineAddExps [] accum = addExpsAccumToExps (Map.toList (Map.fromListWith addExps (sortExpr accum)))
combineAddExps (exp:exps) accum =
    case exp of
        (ACOperation Mul exps') -> combineAddExps exps ((extractConstant exps'):accum)
        _ -> combineAddExps exps (([exp],Constant 1):accum)

sortExpr :: [([Expression],Expression)] -> [([Expression],Expression)]
sortExpr [] = []
sortExpr (pair@(exps,exp):pairs) = ((sort exps),exp):(sortExpr pairs)

addExpsAccumToExps :: [([Expression],Expression)] -> [Expression]
addExpsAccumToExps [] = []
addExpsAccumToExps (pair@(exps,c):pairs)
    | c == Constant 1 = (ACOperation Mul exps):(addExpsAccumToExps pairs)
    | otherwise = (ACOperation Mul (c:exps)):(addExpsAccumToExps pairs)

extractConstant :: [Expression] -> ([Expression],Expression)
extractConstant [] = ([],(Constant 1))
extractConstant (exp:exps) = 
    case exp of
        Constant n -> (exps,Constant n)
        _ -> (exp:(fst res),snd res) 
                where res = extractConstant exps

combineLikeMulTerms :: Expression -> Expression
combineLikeMulTerms (Constant n) = Constant n
combineLikeMulTerms (Reference v) = Reference v
combineLikeMulTerms (BinaryOperation op exp1 exp2) = BinaryOperation op (combineLikeMulTerms exp1) (combineLikeMulTerms exp2)
combineLikeMulTerms (ACOperation Add exps) = ACOperation Add (map combineLikeMulTerms exps)
combineLikeMulTerms (ACOperation Mul exps) = ACOperation Mul (combineMulExps exps [])
combineLikeMulTerms (Application v exp) = Application v (combineLikeMulTerms exp)
combineLikeMulTerms (Derivative v exp) = Derivative v (combineLikeMulTerms exp)

combineMulExps :: [Expression] -> [(Expression, Expression)] -> [Expression]
combineMulExps [] accum = mulExpsAccumToExps (Map.toList (Map.fromListWith addExps accum))
combineMulExps (exp:exps) accum =
    case exp of
        BinaryOperation Pow exp' p -> combineMulExps exps ((exp',p):accum)
        _ -> combineMulExps exps ((exp, Constant 1):accum)

mulExpsAccumToExps :: [(Expression, Expression)] -> [Expression]
mulExpsAccumToExps [] = []
mulExpsAccumToExps ((pair@(exp,p)):pairs)
    | p == Constant 0 = (Constant 1):(mulExpsAccumToExps pairs)
    | p == Constant 1 = exp:(mulExpsAccumToExps pairs)
    | otherwise = (BinaryOperation Pow exp p):(mulExpsAccumToExps pairs)
    

addExps :: Expression -> Expression -> Expression
addExps (ACOperation Add exps1) (ACOperation Add exps2) = ACOperation Add (exps1 ++ exps2)
addExps (ACOperation Add exps1) exp2 = ACOperation Add (exp2:exps1)
addExps exp1 (ACOperation Add exps2) = ACOperation Add (exp1:exps2)
addExps exp1 exp2 = ACOperation Add [exp1,exp2]

-- Simplifying expression to unwrap ACOperations that only have one expression in the expression list
unwrapACOp :: Expression -> Expression
unwrapACOp (ACOperation op (exp:[])) = unwrapACOp exp
unwrapACOp (Constant n) = Constant n
unwrapACOp (Reference v) = Reference v
unwrapACOp (BinaryOperation op exp1 exp2) = BinaryOperation op (unwrapACOp exp1) (unwrapACOp exp2)
unwrapACOp (ACOperation op exps) = ACOperation op (map unwrapACOp exps)
unwrapACOp (Application v exp) = Application v (unwrapACOp exp)
unwrapACOp (Derivative v exp) = Derivative v (unwrapACOp exp)

-- Combine nested ACOperations
combineACOp :: Expression -> Expression
combineACOp (Constant n) = Constant n
combineACOp (Reference v) = Reference v
combineACOp (BinaryOperation op exp1 exp2) = BinaryOperation op (combineACOp exp1) (combineACOp exp2)
combineACOp exp@(ACOperation op exps) = ACOperation op (accumACOpExprs op [] exp)
combineACOp (Application v exp) = Application v (combineACOp exp)
combineACOp (Derivative v exp) = Derivative v (combineACOp exp)

accumACOpExprs :: ACOp -> [Expression] -> Expression -> [Expression]
accumACOpExprs op accum exp@(ACOperation op' exps)= 
    if op == op'
    then (concatMap (accumACOpExprs op accum) exps)
    else (combineACOp exp):accum
accumACOpExprs op accum exp = exp:accum

-- Simplifying expression for d/dx(x) = 1 and d/dx(constant) = 0 
simplifyDerivatives :: Expression -> Expression
simplifyDerivatives (Constant n) = Constant n
simplifyDerivatives (Reference v) = Reference v
simplifyDerivatives (BinaryOperation op exp1 exp2) = BinaryOperation op (simplifyDerivatives exp1) (simplifyDerivatives exp2)
simplifyDerivatives (ACOperation op exps) = ACOperation op (map simplifyDerivatives exps)
simplifyDerivatives (Application v exp) = Application v (simplifyDerivatives exp)
simplifyDerivatives d@(Derivative v (Reference w)) = if v == w then (Constant 1) else (Constant 0)
simplifyDerivatives (Derivative v (Constant n)) = Constant 0
simplifyDerivatives (Derivative v exp) = Derivative v (simplifyDerivatives exp)

-- Simplifying function for basic math (+,-,*,^)
simplifyConstMath :: Expression -> Expression
simplifyConstMath (Constant n) = Constant n
simplifyConstMath (Reference v) = Reference v
simplifyConstMath e@(BinaryOperation op (Constant n) (Constant m)) = 
    case op of
        Sub -> Constant (n-m)
        Pow -> if ((denominator m) == 1) 
               then 
                   if m >= 0 then Constant (n^(numerator m)) 
                   else (BinaryOperation Div (Constant 1) (Constant (n^(-1*(numerator m)))))
                else e
        Div -> Constant (n/m)
simplifyConstMath (BinaryOperation op exp1 exp2) = BinaryOperation op (simplifyConstMath exp1) (simplifyConstMath exp2)
simplifyConstMath (ACOperation Add exps) = 
    case simplified of
        [] -> Constant 0
        _ -> ACOperation Add simplified
        where simplified = (simplifyACOp Add exps 0)
simplifyConstMath (ACOperation Mul exps) = 
    case simplified of
        [] -> Constant 1
        _ -> if (Constant 0) `elem` exps then (Constant 0) else ACOperation Mul simplified
        where simplified = (simplifyACOp Mul exps 1)
simplifyConstMath (Application v exp) = Application v (simplifyConstMath exp)
simplifyConstMath (Derivative v exp) = Derivative v (simplifyConstMath exp)
                    

-- Helper function to simplify constants in ACOperations
simplifyACOp :: ACOp -> [Expression] -> Rational -> [Expression]
simplifyACOp op [] accum =
    case op of
        Add -> if accum /= 0 then [Constant accum] else []
        Mul -> if accum /= 1 then [Constant accum] else []
simplifyACOp op (exp:exps) accum =
    case exp of
        Constant n -> 
            case op of
                Mul -> if n == 0 then [Constant 0] else simplifyACOp op exps (evalACOpConsts op n accum)
                Add -> simplifyACOp op exps (evalACOpConsts op n accum)
        _ -> (simplifyConstMath exp):(simplifyACOp op exps accum)

-- Helper funciton to evaluate ACOperation of 2 numbers
evalACOpConsts :: ACOp -> Rational -> Rational -> Rational
evalACOpConsts op n m =
    case op of
        Add -> n + m
        Mul -> n * m