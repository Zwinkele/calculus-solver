module Derivation where

import Structures
import Control.Monad
import Data.Maybe
import Data.List

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


simplify :: Calculation -> Calculation
simplify calc = simplifyStep calc "Simplify" (combineACOp . convertSubtraction . simplifyDerivatives . simplifyConstMath . unwrapACOp)
    --simplifyStep (simplifyStep calc "Simplify Derivatives" simplifyDerivatives) "Simplify Constant Math" simplifyConstMath 

-- Creates a simplification step using a simplifying function
simplifyStep :: Calculation -> String -> (Expression -> Expression) -> Calculation
simplifyStep (Calc exp steps) stepName simplifyFunc = Calc exp (steps ++ [Step stepName (simplifyFuncRepeat simplifyFunc (getExpFromStep (last steps)))])

getExpFromStep :: Step -> Expression
getExpFromStep (Step s exp) = exp

-- Repeatedly call the simplifyFunc until nothing changes
simplifyFuncRepeat :: (Expression -> Expression) -> Expression -> Expression
simplifyFuncRepeat simplifyFunc exp =
    if exp /= newExp
        then simplifyFuncRepeat simplifyFunc newExp
        else exp
    where newExp = simplifyFunc exp

-- Simplifying expression to convert subtraction to adding a negative
convertSubtraction :: Expression -> Expression 
convertSubtraction (BinaryOperation Sub e1@(ACOperation Add exps1) e2) = ACOperation Add (exps1 ++ [ACOperation Mul [(Constant (negate 1)), e2]])
convertSubtraction (BinaryOperation Sub e1 e2) = ACOperation Add [e1, ACOperation Mul [(Constant (negate 1)), e2]]
convertSubtraction (Constant n) = Constant n
convertSubtraction (Reference v) = Reference v
convertSubtraction (BinaryOperation op exp1 exp2) = BinaryOperation op (convertSubtraction exp1) (convertSubtraction exp2)
convertSubtraction (ACOperation op exps) = ACOperation op (map convertSubtraction exps)
convertSubtraction (Application v exp) = Application v (convertSubtraction exp)
convertSubtraction (Derivative v exp) = Derivative v (convertSubtraction exp)

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
simplifyDerivatives d@(Derivative v (Reference w)) = if v == w then (Constant 1) else d
simplifyDerivatives (Derivative v (Constant n)) = Constant 0
simplifyDerivatives (Derivative v exp) = Derivative v (simplifyDerivatives exp)

-- Simplifying function for basic math (+,-,*,^)
simplifyConstMath :: Expression -> Expression
simplifyConstMath (Constant n) = Constant n
simplifyConstMath (Reference v) = Reference v
simplifyConstMath e@(BinaryOperation op (Constant n) (Constant m)) = 
    case op of
        Sub -> Constant (n-m)
        Pow -> Constant (n^m)
        Div -> e
simplifyConstMath (BinaryOperation op exp1 exp2) = BinaryOperation op (simplifyConstMath exp1) (simplifyConstMath exp2)
simplifyConstMath (ACOperation Add exps) = ACOperation Add (simplifyACOp Add exps 0)
simplifyConstMath (ACOperation Mul exps) = if (Constant 0) `elem` exps then (Constant 0) else ACOperation Mul (simplifyACOp Mul exps 1)
simplifyConstMath (Application v exp) = Application v (simplifyConstMath exp)
simplifyConstMath (Derivative v exp) = Derivative v (simplifyConstMath exp)

-- Helper function to simplify constants in ACOperations
simplifyACOp :: ACOp -> [Expression] -> Int -> [Expression]
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

-- Helper funciton to evaluate ACOperation of 2 integers
evalACOpConsts :: ACOp -> Int -> Int -> Int
evalACOpConsts op n m =
    case op of
        Add -> n + m
        Mul -> n * m