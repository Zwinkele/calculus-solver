module Derivation where

import Structures

calculate :: [Law] -> Expression -> Calculation
calculate laws exp = Calc exp (calculateSteps laws exp)

calculateSteps :: [Law] -> Expression -> [Step]
calculateSteps laws exp = 
    case s of
        Nothing -> []
        Just (Step name newExp) -> (Step name newExp):(calculateSteps laws newExp)
    where s = step laws exp

step :: [Law] -> Expression -> Maybe Step
step [] exp = Nothing
step (law:laws) exp = case e of
                        Nothing -> step laws exp
                        _ -> e
                    where e = rewrite law exp

rewrite :: Law -> Expression -> Maybe Step
rewrite (Law name vars (pattern, replacement)) exp = 
    case newExp of
        Nothing -> Nothing
        (Just newExp') -> Just (Step name newExp')
        where newExp = findMatch f exp
              f exp = case substitution of 
                    Nothing -> Nothing
                    (Just sub) -> Just (apply sub replacement)
                    where substitution = match pattern vars exp
        

findMatch :: (Expression -> Maybe Expression) -> Expression -> Maybe Expression
findMatch f exp =
    case e of
        Nothing -> case exp of
            Constant n -> Nothing
            Reference var -> Nothing
            BinaryOperation op exp1 exp2 -> case match1 of
                Nothing -> case match2 of
                    Nothing -> Nothing
                    (Just matchedExp) -> Just (BinaryOperation op exp1 matchedExp)
                    where match2 = findMatch f exp2
                (Just matchedExp) -> Just (BinaryOperation op matchedExp exp2)
                where match1 = findMatch f exp1
            ACOperation op exps -> case matchedList of
                Nothing -> Nothing
                (Just newExps) -> Just (ACOperation op newExps)
                where matchedList = matchList f exps
            Application v exp' -> case match1 of
                Nothing -> Nothing
                (Just newExp) -> Just (Application v newExp)
                where match1 = f exp'
            Derivative v exp' -> case match1 of
                Nothing -> Nothing
                (Just newExp) -> Just (Derivative v newExp)
                where match1 = f exp'
        _ -> e
        where e = f exp

matchList :: (Expression -> Maybe Expression) -> [Expression] -> Maybe [Expression]
matchList f [] = Nothing
matchList f (exp:exps) = 
    case match of
        Nothing -> 
            case matchedList of
                Nothing -> Nothing
                (Just ml) -> Just (exp:ml)
            where matchedList = matchList f exps
        (Just newExp) -> Just (newExp:exps)
    where match = f exp


-- returns a substitution if the top-level expression matches the pattern
match :: Expression -> [Variable] -> Expression -> Maybe Substitution
match pattern vars exp = 
    case pattern of
        Constant n -> case exp of
            Constant m -> if m == n then (Just []) else Nothing
            _ -> Nothing
        Reference (Variable name) -> Just [(name, exp)]
        BinaryOperation op exp1 exp2 -> case exp of
            BinaryOperation op' exp1' exp2' -> 
                if (op == op') && (matchExp1 /= Nothing) && (matchExp2 /= Nothing)
                then (++) <$> matchExp1 <*> matchExp2
                else Nothing
                where matchExp1 = match exp1 vars exp1'
                      matchExp2 = match exp2 vars exp2'
        ACOperation op exps -> case exp of
            ACOperation op' exps' ->
                if op == op'
                then matchExps
                else Nothing
                where matchExps = fmap concat (sequence (map (\(a,b) -> match a vars b) (zip exps exps')))
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
subVariable ((varName,exp):restOfSubs) v@(Variable name) =
    if varName == name
    then exp
    else subVariable restOfSubs v