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
    where match = f exp


-- returns a substitution if the top-level expression matches the pattern
match :: Expression -> [Variable] -> Expression -> Maybe Substitution
match pattern vars exp = 
    case pattern of
        Constant n -> case exp of
            Constant m -> if m == n then (Just []) else Nothing
            _ -> Nothing
        Reference v -> 
            if v `elem` vars
            then Just [(v, exp)]
            else case exp of
                Reference v' ->
                    if v == v'
                    then (Just [])
                    else Nothing
                _ -> Nothing
        BinaryOperation op exp1 exp2 -> case exp of
            BinaryOperation op' exp1' exp2' -> 
                if (op == op')
                then (++) <$> (match exp1 vars exp1') <*> (match exp2 vars exp2')
                else Nothing
            _ -> Nothing
        ACOperation op exps -> case exp of
            ACOperation op' exps' ->
                if op == op'
                then fmap concat (sequence (map (\(a,b) -> match a vars b) (zip exps exps')))
                else Nothing
            _ -> Nothing
        Application v innerPattern -> case exp of
            Application v' innerExp ->
                if v == v'
                then match innerPattern vars innerExp
                else Nothing
            _ -> Nothing
        Derivative v innerPattern -> case exp of
            Derivative v' innerExp ->
                if v == v'
                then match innerPattern vars innerExp
                else Nothing
            _ -> Nothing

apply :: Substitution -> Expression -> Expression
apply sub replacement = 
    case replacement of
        Constant n -> Constant n
        Reference v -> subVariable sub v
        BinaryOperation op exp1 exp2 -> BinaryOperation op (apply sub exp1) (apply sub exp2)
        ACOperation op exps -> ACOperation op (map (apply sub) exps)
        Application v exp -> Application v (apply sub exp)
        Derivative v exp -> Derivative v (apply sub exp)

subVariable :: Substitution -> Variable -> Expression
subVariable [] v = Reference v
subVariable ((v',exp):restOfSubs) v =
    if v == v'
    then exp
    else subVariable restOfSubs v